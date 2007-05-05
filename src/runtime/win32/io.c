/**
 * @file 
 * @brief I/O処理(実行時ライブラリ)
 *
 * @author Kenta HATTORI
 * @date   2006/04/26
 * $Id: io.c,v 1.8 2006/08/07 09:07:51 hattori Exp $
 */
#include <stdio.h>
#include "prcrt.h"
#include "proc.h"
#include "timer.h"
#include "perr.h"
#include "sock.h"
#include "wave.h"
#include "exec.h"
#include "strerror.h"
#include "io.h"

ioq_t __prc__ioq;

/* pselectで待つIO */
static ioq_t io_mrdq;
static ioq_t io_mwrq;

static int aio_count;

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
	ioent_delete2(io->handle);
	return;
    }
    len -= io->offset;
    if (io->bufsz < len) {
	len = io->bufsz;
    }
    memset(&io->ctlblk, 0, sizeof(OVERLAPPED));
    memcpy((void *)io->buf, STRPTR(val)+io->offset, len);
    if (WriteFile(io->handle, io->buf, len, NULL, &io->ctlblk) == FALSE) {
	perr(PERR_SYSTEM, "WriteFile", StrError(GetLastError()), __FILE__, __LINE__);
    }
}

static void io_recv(ioent_t *io) {
    if (ReadFile(io->handle, io->buf, io->bufsz, NULL, &io->ctlblk) == FALSE) {
	perr(PERR_SYSTEM, "ReadFile", StrError(GetLastError()), __FILE__, __LINE__);
    }
}

static void io_accept(ioent_t *io) {
    proc_t *prc;
    event_t *evt;
    SOCKET so;
    // printf("io_accept: enter\n");
    if ((so = accept((SOCKET)io->handle, NULL, NULL)) < 0) {
	perr(PERR_SYSTEM, "accept", StrError(GetLastError()), __FILE__, __LINE__);
    }
    __prc__regs[0] = (int)__chan__();
    __prc__regs[1] = (int)__chan__();
    ioent_create((chan_t *)__prc__regs[0], (HANDLE)so, IO_TYPE_IN,  BUFSIZ);
    ioent_create((chan_t *)__prc__regs[1], (HANDLE)so, IO_TYPE_OUT, BUFSIZ);
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


/* ディスクリプタ集合を初期化する */
static int io_set_events(HANDLE *events) {
    ioent_t *io;
    event_t *evt;
    int count = 0;

    for (io = io_mrdq.tqh_first; io != NULL; io = io->mlink.tqe_next) {
	if ((evt = chin_next(io->chan)) == NULL || (evt->trans == 0 && io->type == IO_TYPE_IN)) {
	    TAILQ_REMOVE(&io_mrdq, io, mlink);
            io->mlink.tqe_prev = NULL;
	    continue;
	}
	events[count++] = io->ctlblk.hEvent;
    }
    for (io = io_mwrq.tqh_first; io != NULL; io = io->mlink.tqe_next) {
	if ((evt = chout_next(io->chan)) == NULL || evt->trans == 0) {
	    TAILQ_REMOVE(&io_mwrq, io, mlink);
            io->mlink.tqe_prev = NULL;
	    continue;
	}
	events[count++] = io->ctlblk.hEvent;
    }

    return count;
}

/**
 * I/O入出力処理 
 */
int io_exec(void) {
    static HANDLE events[MAXIMUM_WAIT_OBJECTS];
    event_t *evt;
    DWORD timo;
    DWORD ret;
    int n;

    /* 多重化IOのイベントリストを初期化 */
    n = io_set_events(events);

    /* タイマーイベントを取得 */
    timo = timer_next();
    if (timo == INFINITE && n == 0 && aio_count == 0) {
	return (int)__stop__;
    }

    ret = WaitForMultipleObjectsEx(n, events, FALSE, timo, TRUE);
    if ((ret >= WAIT_OBJECT_0 && ret - WAIT_OBJECT_0 < n) ||
        (ret >= WAIT_ABANDONED_0 && ret - WAIT_ABANDONED_0 < n)) {
        ioent_t *io;
        /* 多重IOの処理 */
        for (io = io_mrdq.tqh_first; io != NULL; io = io->mlink.tqe_next) {
            if (WaitForSingleObject(io->ctlblk.hEvent, 0) == WAIT_OBJECT_0) {
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
                if (!ResetEvent(io->ctlblk.hEvent)) {
                    perr(PERR_SYSTEM, "ResetEvent", StrError(GetLastError()), __FILE__, __LINE__);
                }
            }
        }
        for (io = io_mwrq.tqh_first; io != NULL; io = io->mlink.tqe_next) {
            if (WaitForSingleObject(io->ctlblk.hEvent, 0) == WAIT_OBJECT_0) {
                if ((evt = chout_next(io->chan)) != NULL) {
                    EV_SET_CANCEL(evt);
                    evt->trans = 0;
                    io_send(io, evt->val);
                }
                TAILQ_REMOVE(&io_mwrq, io, mlink);
                if (!ResetEvent(io->ctlblk.hEvent)) {
                    perr(PERR_SYSTEM, "ResetEvent", StrError(GetLastError()), __FILE__, __LINE__);
                }
            }
        }
    } else if (ret == WAIT_TIMEOUT) {
        /* タイムアウト時 */
        evt = timer_take();
        EV_SET_CANCEL(evt);
        __prc__regs[0] = evt->clos;
        __prc__regs[1] = evt->val;
        return ((int *)evt->clos)[0];
    } else if (ret == -1) {
        perr(PERR_SYSTEM, "WaitForMultipleObjects", StrError(GetLastError()), __FILE__, __LINE__);
    }

    return (int)__disp__;
}

/**
 * I/O処理の初期化
 */
void io_init(void) {
    TAILQ_INIT(&__prc__ioq);
    TAILQ_INIT(&io_mrdq);
    TAILQ_INIT(&io_mwrq);

    /* I/Oチャネルの登録 */
    __prc__stdout = (int)__chan__();
    __prc__stdin  = (int)__chan__();
    __prc__timer  = (int)__chan__();

    ioent_create((chan_t *)__prc__stdout, GetStdHandle(STD_OUTPUT_HANDLE), IO_TYPE_OUT,   BUFSIZ);
    ioent_create((chan_t *)__prc__stdin,  GetStdHandle(STD_INPUT_HANDLE),  IO_TYPE_IN,    BUFSIZ);
    ioent_create((chan_t *)__prc__timer,  0,             IO_TYPE_TIMER, 0);
}

/**
 * IOエントリの新規作成
 */
void ioent_create(chan_t *ch, HANDLE handle, iotype_t type, size_t size) {
    ioent_t *io;

    io = malloc(sizeof(ioent_t)+size);
    if (io == NULL) {
        perr(PERR_OUTOFMEM, __FILE__, __LINE__);
    }

    io->type   = type;
    io->chan   = ch;
    io->handle = handle;
    io->offset = 0;
    io->bufsz  = size;

    memset(&io->ctlblk, 0, sizeof(OVERLAPPED));
    /* 手動リセットイベントを作成 */
    if ((io->ctlblk.hEvent = CreateEvent(NULL, TRUE, FALSE, NULL)) == NULL) {
	perr(PERR_SYSTEM, "CreateEvent", StrError(GetLastError()), __FILE__, __LINE__);
    }

    TAILQ_INSERT_TAIL(&__prc__ioq, io, link);
    io->mlink.tqe_prev = NULL;
    // printf("insert: %p(desc=%d)\n", io, io->handle);

    ch->ioent = io;
}

/**
 * I/Oエントリの削除
 */
void ioent_delete(ioent_t *ioent) {
    ioent_t *io;

    TAILQ_REMOVE(&__prc__ioq, ioent, link);
    ioent->chan->ioent = NULL;
    if (!CloseHandle(ioent->handle)) {
        perr(PERR_SYSTEM, "CloseHandle", StrError(GetLastError()), __FILE__, __LINE__);
    }
    // printf("ioent_delete: %d\n", ioent->handle);

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
void ioent_delete2(HANDLE handle) {
    ioent_t *io, *oio;

    for (io = __prc__ioq.tqh_first; io != NULL; ) {
	if (io->handle == handle) {
	    oio = io;
            io = io->link.tqe_next;
	    ioent_delete(oio);
	} else {
	    io = io->link.tqe_next;
	}
    }
}
