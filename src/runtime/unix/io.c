/**
 * @file 
 * @brief I/O処理(実行時ライブラリ)
 *
 * @author Kenta HATTORI
 * @date   2006/04/26
 * $Id: io.c,v 1.8 2006/08/07 09:07:51 hattori Exp $
 */
#include <stdio.h>
#include <string.h>
#include <setjmp.h>
#include <unistd.h>
#include <signal.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <errno.h>
#include <aio.h>
#include "prcrt.h"
#include "proc.h"
#include "chan.h"
#include "timer.h"
#include "perr.h"
#include "sock.h"
#include "exec.h"
#include "io.h"

ioq_t __prc__ioq;

/* pselectで待つIO */
static ioq_t io_mrdq;
static ioq_t io_mwrq;

#define AIO_MAX 10
static ioent_t *aio_ioent[AIO_MAX];
static int aio_errno[AIO_MAX];
static ssize_t aio_ret[AIO_MAX];
static int aio_count;
static int aio_completion_count;

static sigset_t ss_default;
static sigset_t ss_sigio;
static struct timespec ts_zero;
static sigjmp_buf sj_buf;

#define PRC_MAX(a,b) ((a) > (b) ? (a) : (b))

static void io_send(ioent_t *io, int val) {
    int len;

    /* 非同期IO要求を発行 */
    if ((len = STRLEN(val)) == 0) {
	event_t *evt;
	proc_t *prc;

	__prc__regs[0] = (int)chout_next(io->chan);
	prc = proc();
	evt = (event_t *)__prc__regs[0]; /* GCを考慮しevtを再取得 */
	prc->clos = evt->clos;
     	prc->val  = 0;
        TAILQ_INSERT_TAIL(__prc__rdyq, prc, link);
	ioent_delete2(io->desc);
	return;
    }
    len -= io->offset;
    if (io->bufsz < len) {
	len = io->bufsz;
    }
    memcpy((void *)io->aiocb->aio_buf, STRPTR(val)+io->offset, len);

    io->aiocb->aio_nbytes = len;
    aio_count++;
    if (aio_write(io->aiocb) < 0) {
	perr(PERR_SYSTEM, "aio_write", strerror(errno), __FILE__, __LINE__);
    }
}

static void io_recv(ioent_t *io) {
    io->aiocb->aio_nbytes = io->bufsz;
    aio_count++;
    if (aio_read(io->aiocb) < 0) {
	perr(PERR_SYSTEM, "aio_read", strerror(errno), __FILE__, __LINE__);
    }
}

static void io_accept(ioent_t *io) {
    proc_t *prc;
    event_t *evt;
    int so;
    // printf("io_accept: enter\n");
    if ((so = accept(io->desc, NULL, NULL)) < 0) {
	perr(PERR_SYSTEM, "accept", strerror(errno), __FILE__, __LINE__);
    }
    __prc__regs[0] = (int)__chan__();
    __prc__regs[1] = (int)__chan__();
    ioent_create((chan_t *)__prc__regs[0], so, IO_TYPE_IN,  BUFSIZ);
    ioent_create((chan_t *)__prc__regs[1], so, IO_TYPE_OUT, BUFSIZ);
    __prc__regs[2] = __record__(2);
    ((int*)__prc__regs[2])[0] = __prc__regs[0];
    ((int*)__prc__regs[2])[1] = __prc__regs[1];

    /* 実行可能プロセスをセット */
    prc = proc();
    evt = chin_next(io->chan);
    prc->clos = evt->clos;
    prc->val  = __prc__regs[2];
    TAILQ_INSERT_TAIL(__prc__rdyq, prc, link);
    TAILQ_REMOVE(&io->chan->inq, evt, link);
    //printf("io_accept: leave: %d\n", so);
}

static void aio_completion_handler(int signo, siginfo_t *info, void *context) {
    struct aiocb *req;
    //printf("aio_completion_handler: %d: enter\n", info->si_code);
    if (info->si_signo != SIGRTMIN+SIGIO) {
        perr(PERR_INTERNAL, __FILE__, __LINE__);
    }
    if (info->si_code != SI_ASYNCIO) {
   	return;
    }

    aio_ioent[aio_completion_count] = (ioent_t *)info->si_value.sival_ptr;
    req = aio_ioent[aio_completion_count]->aiocb;
    if ((aio_errno[aio_completion_count] = aio_error(req)) == 0) {
	aio_ret[aio_completion_count] = aio_return(req);
    } else {
        perr(PERR_SYSTEM, "aio_error", strerror(aio_errno[aio_completion_count]), __FILE__, __LINE__);
    }

    //printf("aio_completion_handler: %p: %d: code=%d: leave\n", aio_ioent[aio_completion_count], aio_ioent[aio_completion_count]->desc, info->si_code);
    aio_completion_count++;

    siglongjmp(sj_buf, 1);
}

/**
 * I/O処理の初期化
 */
void io_init(void) {
    struct sigaction sig_act;

    TAILQ_INIT(&__prc__ioq);
    TAILQ_INIT(&io_mrdq);
    TAILQ_INIT(&io_mwrq);

    sigemptyset(&ss_sigio);
    sigaddset(&ss_sigio, SIGRTMIN+SIGIO);
    sigprocmask(SIG_BLOCK, &ss_sigio, &ss_default);
    
    /* シグナルハンドラを登録 */
    sigemptyset(&sig_act.sa_mask);
    sig_act.sa_flags = SA_SIGINFO;
    sig_act.sa_sigaction = aio_completion_handler;

    if (sigaction(SIGRTMIN+SIGIO, &sig_act, NULL) < 0) {
	perr(PERR_SYSTEM, "sigaction", strerror(errno), __FILE__, __LINE__);
    }

    ts_zero.tv_sec  = 0;
    ts_zero.tv_nsec = 0;

    /* I/Oチャネルの登録 */
    __prc__stdout = (int)__chan__();
    __prc__stdin  = (int)__chan__();
    __prc__timer  = (int)__chan__();

    ioent_create((chan_t *)__prc__stdout, STDOUT_FILENO, IO_TYPE_OUT,   BUFSIZ);
    ioent_create((chan_t *)__prc__stdin,  STDIN_FILENO,  IO_TYPE_IN,    BUFSIZ);
    ioent_create((chan_t *)__prc__timer,  0,             IO_TYPE_TIMER, 0);
}

/**
 * IOエントリの新規作成
 */
void ioent_create(chan_t *ch, int desc, iotype_t type, size_t size) {
    ioent_t *io;

    io = malloc(sizeof(ioent_t)+sizeof(struct aiocb)+size);
    if (io == NULL) {
        perr(PERR_OUTOFMEM, __FILE__, __LINE__);
    }
    io->aiocb = (struct aiocb *)(io+1);
    memset(io->aiocb, 0, sizeof(struct aiocb));
    io->bufsz  = size;
    io->chan   = ch;
    io->type   = type;
    io->offset = 0;
    io->mlink.tqe_prev = NULL;
    io->aiocb->aio_fildes = io->desc = desc;
    io->aiocb->aio_buf    = (char *)(io->aiocb+1);

    io->aiocb->aio_sigevent.sigev_notify = SIGEV_SIGNAL;
    io->aiocb->aio_sigevent.sigev_signo  = SIGRTMIN+SIGIO;
    io->aiocb->aio_sigevent.sigev_value.sival_ptr = io;

    TAILQ_INSERT_TAIL(&__prc__ioq, io, link);
    // printf("insert: %p(desc=%d)\n", io, io->desc);

    ch->ioent = io;
}

/**
 * I/Oエントリの削除
 */
void ioent_delete(ioent_t *ioent) {
    ioent_t *io;

    TAILQ_REMOVE(&__prc__ioq, ioent, link);
    ioent->chan->ioent = NULL;
    close(ioent->desc);
    // printf("ioent_delete: %d\n", ioent->desc);

    for (io = io_mrdq.tqh_first; io != NULL; io = io->mlink.tqe_next) {
	if (io == ioent) {
	    TAILQ_REMOVE(&io_mrdq, ioent, mlink);
	}
    }
    for (io = io_mwrq.tqh_first; io != NULL; io = io->mlink.tqe_next) {
	if (io == ioent) {
	    TAILQ_REMOVE(&io_mwrq, ioent, mlink);
	}
    }
    free(ioent);
}
void ioent_delete2(int desc) {
    ioent_t *io, *oio;

    for (io = __prc__ioq.tqh_first; io != NULL; ) {
	if (io->desc == desc) {
	    oio = io;
            io = io->link.tqe_next;
	    ioent_delete(oio);
	} else {
	    io = io->link.tqe_next;
	}
    }
}

/* 非同期I/O完了時の処理 */
static void io_complete(ioent_t *ioent, ssize_t len) {
    event_t *evt;
    proc_t *prc;

    if (len < 0) {
        perr(PERR_INTERNAL, __FILE__, __LINE__);
    }

    aio_count--;
    switch (ioent->type) {
    case IO_TYPE_IN:
	/* 受信データをコピー */
	__prc__regs[0] = __string__(len, (void *)ioent->aiocb->aio_buf);
	prc = proc();
	evt = chin_next(ioent->chan);
	prc->clos = evt->clos;
	prc->val  = __prc__regs[0];
        TAILQ_INSERT_TAIL(__prc__rdyq, prc, link);
        TAILQ_REMOVE(&ioent->chan->inq, evt, link);

	ioent->aiocb->aio_offset += len;
	/* IO待ちプロセスが無ければ終了 */
	if ((evt = chin_next(ioent->chan)) == NULL) {
	    if (len == 0) {
		ioent_delete2(ioent->desc);
	    }
	    return;
	}

	if (evt->trans == 0) {
	    io_recv(ioent);
	} else {
	    TAILQ_INSERT_TAIL(&io_mrdq, ioent, mlink);
	}

	break;
    case IO_TYPE_OUT:
	/* 未送信データが残っていれば，再度，IO要求を発行 */
	ioent->offset += len;
	ioent->aiocb->aio_offset += len;
	__prc__regs[0] = (int)chout_next(ioent->chan);
	evt = (event_t *)__prc__regs[0];
	if (STRLEN(evt->val) > ioent->offset) {
	    io_send(ioent, evt->val);
	    return;
	}

	prc = proc();
	evt = (event_t *)__prc__regs[0]; /* GCを考慮しevtを再取得 */
	prc->clos = evt->clos;
     	prc->val  = 0;
        TAILQ_INSERT_TAIL(__prc__rdyq, prc, link);
        TAILQ_REMOVE(&ioent->chan->outq, evt, link);

	/* IO待ちプロセスが無ければ終了 */
	if ((evt = chout_next(ioent->chan)) == NULL) {
	    return;
	}
	/* 次のIO待ちプロセスの処理 */
	if (evt->trans == 0) {
	    ioent->offset = 0;
	    io_send(ioent, evt->val);
	} else {
	    TAILQ_INSERT_TAIL(&io_mwrq, ioent, mlink);
	}
	break;
    default:
	perr(PERR_INTERNAL, __FILE__, __LINE__);
	break;
    }
}

/* ディスクリプタ集合を初期化する */
static int io_set_fds(fd_set *rfds, fd_set *wfds) {
    ioent_t *io;
    event_t *evt;
    int max = 0;

    FD_ZERO(rfds);
    FD_ZERO(wfds);

    for (io = io_mrdq.tqh_first; io != NULL; io = io->mlink.tqe_next) {
	if ((evt = chin_next(io->chan)) == NULL || (evt->trans == 0 && io->type == IO_TYPE_IN)) {
	    TAILQ_REMOVE(&io_mrdq, io, mlink);
            io->mlink.tqe_prev = NULL;
	    continue;
	}

	FD_SET(io->desc, rfds);
	max = PRC_MAX(max, io->desc);
    }
    for (io = io_mwrq.tqh_first; io != NULL; io = io->mlink.tqe_next) {
	if ((evt = chout_next(io->chan)) == NULL || evt->trans == 0) {
	    TAILQ_REMOVE(&io_mwrq, io, mlink);
            io->mlink.tqe_prev = NULL;
	    continue;
	}
	FD_SET(io->desc, wfds);
	max = PRC_MAX(max, io->desc);
    }

    return max;
}

/**
 * I/O入出力処理 
 */
int io_exec(void) {
    const struct timespec *ts;
    siginfo_t si;
    event_t *evt;
    fd_set rfds, wfds;
    int i;
    int n;
    int ret;

    /* ディスクリプタ集合を初期化 */
    n = io_set_fds(&rfds, &wfds);

    /* タイマーイベントを取得 */
    ts = timer_next();
    if (ts == NULL && n == 0 && aio_count == 0) {
	return (int)__stop__;
    }
    if (sigsetjmp(sj_buf, 1) == 0) {
	//printf("aio_count = %d\n", aio_count);
	ret = pselect(n+1, &rfds, &wfds, NULL, ts, &ss_default);
	if (ret < 0) {
            if (errno == EINTR) {
	        goto interrupted;
	    }
	    perr(PERR_SYSTEM, "pselect", strerror(errno), __FILE__, __LINE__);
	} else if (ret == 0) {
	    /* タイムアウト時 */
	    evt = timer_take();

            EV_SET_CANCEL(evt);
	    __prc__regs[0] = evt->clos;
            __prc__regs[1] = evt->val;
	    return ((int *)evt->clos)[0];
	} else {
	    ioent_t *io;

	    /* 多重IOの処理 */
	    for (io = io_mrdq.tqh_first; io != NULL; io = io->mlink.tqe_next) {
		if (FD_ISSET(io->desc, &rfds)) {
		    if ((evt = chin_next(io->chan)) != NULL) {
			if (io->type == IO_TYPE_ACCEPT) {
			    io_accept(io);
			    EV_SET_CANCEL(evt);
			} else {
			    EV_SET_CANCEL(evt);
			    evt->trans = 0;
			    io_recv(io);
			}
		    }
		    TAILQ_REMOVE(&io_mrdq, io, mlink);
		}
	    }
	    for (io = io_mwrq.tqh_first; io != NULL; io = io->mlink.tqe_next) {
		if (FD_ISSET(io->desc, &wfds)) {
		    if ((evt = chout_next(io->chan)) != NULL) {
			EV_SET_CANCEL(evt);
			evt->trans = 0;
			io_send(io, evt->val);
		    }
		    TAILQ_REMOVE(&io_mwrq, io, mlink);
		}
	    }
	}
    }

interrupted:
    /* IO完了シグナルを処理 */
    for (i = 0; i < aio_completion_count; i++) {
        if (aio_errno[i] == 0) {
	    io_complete(aio_ioent[i], aio_ret[i]);
        }
    }
    while (sigtimedwait(&ss_sigio, &si, &ts_zero) == SIGRTMIN+SIGIO) {
        ioent_t *ioent = (ioent_t *)si.si_value.sival_ptr;
        int err = aio_error(ioent->aiocb);
	//printf("sigtimedwait: %d\n", si.si_code);
        if (err == 0) {
	    io_complete(ioent, aio_return(ioent->aiocb));
	}
    }
    aio_completion_count = 0;

    return (int)__disp__;
}

/**
 * IOチャネル出力時の処理 
 */
void io_chout(ioent_t *io, event_t *evt) {
    switch (io->type) {
    case IO_TYPE_OUT:
	/* 既にIO発行済みであれば何もしない */
	if (chout_next(io->chan) != NULL) {
	    TAILQ_INSERT_TAIL(&io->chan->outq, evt, link);
	    return;
	}
	TAILQ_INSERT_TAIL(&io->chan->outq, evt, link);
	io->offset = 0;
	if (evt->trans == 0) {
	    io_send(io, evt->val);
	} else {
            if (io->mlink.tqe_prev == NULL) {
	        TAILQ_INSERT_TAIL(&io_mwrq, io, mlink);
 	    }
	}
	break;
    case IO_TYPE_TIMER:
	timer_add(evt);
	break;
    default:
	perr(PERR_INTERNAL, __FILE__, __LINE__);
	break;
    }
}

/**
 * IOチャネル入力時の処理 
 */
void io_chin(ioent_t *io, event_t *evt) {
    switch (io->type) {
    case IO_TYPE_IN:
	/* 既にIO発行済みであれば何もしない */
	if (chin_next(io->chan) != NULL) {
	    return;
	}
	if (evt->trans == 0) {
	    io_recv(io);
	} else {
            if (io->mlink.tqe_prev == NULL) {
	        TAILQ_INSERT_TAIL(&io_mrdq, io, mlink);
            } 
	}
	break;
    case IO_TYPE_ACCEPT:
	/* 既にIO発行済みであれば何もしない */
	if (chin_next(io->chan) != NULL) {
	    return;
	}
        if (io->mlink.tqe_prev == NULL) {
	    TAILQ_INSERT_TAIL(&io_mrdq, io, mlink);
        }
	break;
    default:
	perr(PERR_INTERNAL, __FILE__, __LINE__);
	break;
    }
}

