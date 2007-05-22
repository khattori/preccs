/**
 * @file 
 * @brief I/O処理(実行時ライブラリ)
 *
 * @author Kenta HATTORI
 * @date   2006/04/26
 * $Id: io.c,v 1.8 2006/08/07 09:07:51 hattori Exp $
 */
#include <stdio.h>
#include <stddef.h>
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
ioq_t __prc__mioq;

int aio_count;

static sigset_t ss_default;
static sigset_t ss_sigio;
static struct timespec ts_zero;
static sigjmp_buf sj_buf;

#define PRC_MAX(a,b) ((a) > (b) ? (a) : (b))

/**
 * 非同期IOの完了を待つ
 */
void io_wait_cs(void) {
    perr(PERR_INTERNAL, __FILE__, __LINE__);
}
void io_enter_cs(void) {
    perr(PERR_INTERNAL, __FILE__, __LINE__);
}
void io_leave_cs(void) {
    perr(PERR_INTERNAL, __FILE__, __LINE__);
}

/*
 * IO読み込みの完了処理
 */
void io_read_complete(ioent_t *io, int len) {
    proc_t *prc;
    event_t *evt;

    io->ctlblk.aio_offset += len;
    __prc__regs[0] = __string__(len, (void *)io->buf);
    prc = proc();
    evt = chin_next(io->chan);
    prc->clos = evt->clos;
    prc->val  = __prc__regs[0];
    TAILQ_INSERT_TAIL(__prc__rdyq, prc, link);
    TAILQ_REMOVE(&io->chan->inq, evt, link);
}

/*
 * IO書き込みの完了処理
 */
void io_write_complete(ioent_t *io, int len) {
    event_t *evt;
    proc_t *prc;

    io->ctlblk.aio_offset += len;
    prc = proc();
    evt = (event_t*)chout_next(io->chan);
    prc->clos = evt->clos;
    prc->val  = 0;
    TAILQ_INSERT_TAIL(__prc__rdyq, prc, link);
    TAILQ_REMOVE(&io->chan->outq, evt, link);
}

static void io_complete(ioent_t *io) {
    event_t *evt;
    int len;

    aio_count--;
    if (aio_error(&io->ctlblk) != 0) {
	return;
    }
    len = aio_return(&io->ctlblk);
    switch (io->iotype) {
    case IOT_INPUT:
	io_read_complete(io, len);
	if ((evt = chin_next(io->chan)) != NULL) {
	    io->iof(io, evt, 0);
	}
	break;
    case IOT_OUTPUT:
	io_write_complete(io, len);
	if ((evt = chout_next(io->chan)) != NULL) {
	    io->iof(io, evt, 0);
	}
	break;
    default:
	perr(PERR_INTERNAL, __FILE__, __LINE__);
    }
}

static void aio_completion_handler(int signo, siginfo_t *info, void *context) {
//    printf("aio_completion_handler: enter: (si_signo=%d,si_code=%d)\n", info->si_signo, info->si_code);
//    fflush(stdout);

    if (info->si_signo != SIGRTMIN+SIGIO) {
        perr(PERR_INTERNAL, __FILE__, __LINE__);
    }
    if (info->si_code != SI_ASYNCIO) {
   	return;
    }

    io_complete((ioent_t *)info->si_value.sival_ptr);

    siglongjmp(sj_buf, 1);
}

/**
 * IOチャネル入力時の処理 
 */
void io_input(ioent_t *io, event_t *evt, int exec) {
    if (evt->trans == 0) {
	aio_count++;
	if (aio_read(&io->ctlblk) < 0) {
	    perr(PERR_SYSTEM, "aio_read", strerror(errno), __FILE__, __LINE__);
	}
	return;
    }
    TAILQ_INSERT_TAIL(&__prc__mioq, io, mlink);
}

/**
 * IOチャネル出力時の処理 
 */
void io_output(ioent_t *io, event_t *evt, int exec) {
    if (evt->trans == 0) {
	aio_count++;
	io->ctlblk.aio_buf = STRPTR(evt->val);
	io->ctlblk.aio_nbytes = STRLEN(evt->val);
	if (aio_write(&io->ctlblk) < 0) {
	    perr(PERR_SYSTEM, "aio_write", strerror(errno), __FILE__, __LINE__);
	}
	return;
    }
    TAILQ_INSERT_TAIL(&__prc__mioq, io, mlink);
}

static void io_tmout(ioent_t *io, event_t *evt, int exec) {
    TAILQ_REMOVE(&io->chan->outq, evt, link);
    timer_add(evt);
}

/**
 * I/O処理の初期化
 */
void io_init(void) {
    struct sigaction sig_act;

    TAILQ_INIT(&__prc__ioq);
    TAILQ_INIT(&__prc__mioq);

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

    ioent_create((chan_t *)__prc__stdout, STDOUT_FILENO, IOT_OUTPUT, io_output, BUFSIZ);
    ioent_create((chan_t *)__prc__stdin, STDIN_FILENO, IOT_INPUT, io_input, BUFSIZ);
    ioent_create((chan_t *)__prc__timer, 0, IOT_OUTPUT, io_tmout, 0);
}

/* ディスクリプタ集合を初期化する */
static int io_set_fds(fd_set *rfds, fd_set *wfds) {
    ioent_t *io;
    int max = 0;

    FD_ZERO(rfds);
    FD_ZERO(wfds);

    for (io = __prc__mioq.tqh_first; io != NULL; io = io->mlink.tqe_next) {
	switch (io->iotype) {
	case IOT_INPUT:
	    if (chin_next(io->chan) == NULL) {
		/* キャンセルされた場合 */
		TAILQ_REMOVE(&__prc__mioq, io, mlink);
		io->mlink.tqe_prev = NULL;
		continue;
	    }
	    FD_SET(io->handle, rfds);
	    break;
	case IOT_OUTPUT:
	    if (chout_next(io->chan) == NULL) {
		/* キャンセルされた場合 */
		TAILQ_REMOVE(&__prc__mioq, io, mlink);
		io->mlink.tqe_prev = NULL;
		continue;
	    }
	    FD_SET(io->handle, wfds);
	    break;
	default:
	    perr(PERR_INTERNAL, __FILE__, __LINE__);
	    break;
	}
	max = PRC_MAX(max, io->handle);
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
	    for (io = __prc__mioq.tqh_first; io != NULL; io = io->mlink.tqe_next) {
		if (FD_ISSET(io->handle, &rfds) || FD_ISSET(io->handle, &wfds)) {
		    TAILQ_REMOVE(&__prc__mioq, io, mlink);
		    io->mlink.tqe_prev = NULL;

		    if ((io->iotype == IOT_INPUT && (evt = chin_next(io->chan)) != NULL) ||
			(io->iotype == IOT_OUTPUT && (evt = chout_next(io->chan)) != NULL)) {
			EV_SET_CANCEL(evt);
			evt->trans = 0;
			io->iof(io, evt, !0);
		    }
		}
	    }
	}
	
    }

interrupted:
    /* IO完了シグナルを処理 */
    while (sigtimedwait(&ss_sigio, &si, &ts_zero) == SIGRTMIN+SIGIO) {
	io_complete((ioent_t *)si.si_value.sival_ptr);
    }

    return (int)__disp__;
}

/**
 * IOエントリの新規作成
 */
ioent_t *ioent_create(chan_t *ch, int handle, iotype_t iotype, iof_t iof, size_t size) {
    ioent_t *io;

    io = malloc(sizeof(ioent_t)+size);
    if (io == NULL) {
        perr(PERR_OUTOFMEM, __FILE__, __LINE__);
	return NULL;
    }
    io->iotype = iotype;
    io->iof    = iof;
    io->chan   = ch;
    io->data   = NULL;
    io->handle = handle;
    io->offset = 0;
    io->bufsz  = size;

    memset(&io->ctlblk, 0, sizeof(struct aiocb));
    io->ctlblk.aio_fildes = handle;
    io->ctlblk.aio_buf    = (char *)io->buf;
    io->ctlblk.aio_sigevent.sigev_notify = SIGEV_SIGNAL;
    io->ctlblk.aio_sigevent.sigev_signo  = SIGRTMIN+SIGIO;
    io->ctlblk.aio_sigevent.sigev_value.sival_ptr = io;

    TAILQ_INSERT_TAIL(&__prc__ioq, io, link);
    io->mlink.tqe_prev = NULL;

    ch->ioent = io;

    return io;
}

/**
 * I/Oエントリの削除
 */
void ioent_delete(ioent_t *ioent) {
    TAILQ_REMOVE(&__prc__ioq, ioent, link);
    ioent->chan->ioent = NULL;
    free(ioent);
}

