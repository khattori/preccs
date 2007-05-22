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
ioq_t __prc__mioq; /* WaitForMultipleObjectsで待つIO */

int aio_count;

/*
 * IO読み込みの完了処理
 */
void io_read_complete(ioent_t *io, int len) {
    proc_t *prc;
    event_t *evt;

    io->ctlblk.Offset += len;
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

    io->ctlblk.Offset += len;
    prc = proc();
    evt = (event_t*)chout_next(io->chan);
    prc->clos = evt->clos;
    prc->val  = 0;
    TAILQ_INSERT_TAIL(__prc__rdyq, prc, link);
    TAILQ_REMOVE(&io->chan->outq, evt, link);
}

static HANDLE stdin_thread_handle;
static DWORD WINAPI stdin_thread(LPVOID args) {
    ioent_t *io = (ioent_t *)args;

//    printf("thrConsolePeek: enter\n");
//    fflush(stdout);

    if (!ReadFile(io->handle, io->buf, io->bufsz, (LPDWORD)&io->offset, NULL)) {
        perr(PERR_SYSTEM, "ReadFile", StrError(GetLastError()), __FILE__, __LINE__);
        return -1;
    }
    stdin_thread_handle = NULL;
    if (!SetEvent(io->ctlblk.hEvent)) {
        perr(PERR_SYSTEM, "SetEvent", StrError(GetLastError()), __FILE__, __LINE__);
        return -1;
    }

//    printf("thrConsolePeek: leave\n");
//    fflush(stdout);

    return 0;
}
/**
 * 標準コンソール入力IOの処理
 */
static void io_stdin(ioent_t *io, event_t *evt, int exec) {
//    printf("io_stdin: enter(exec=%d,len=%d)\n", exec, io->offset);
//    fflush(stdout);

    /* 受信データが存在し，イベントが通知された場合 */
    if (exec) {
        /* 受信データをコピー */
        io_read_complete(io, io->offset);
        io->offset = 0;
        /* IO待ちプロセスが無ければ終了 */
        if ((evt = chin_next(io->chan)) == NULL) {
            return;
        }
        io->iof(io, evt, 0);
        return;
    }

    /* スレッド未起動の場合 */
    if (io->offset == 0 && stdin_thread_handle == NULL) {
        stdin_thread_handle = CreateThread(NULL, 0, stdin_thread, ((chan_t *)__prc__stdin)->ioent, 0, NULL);
        if (stdin_thread_handle == NULL) {
            perr(PERR_SYSTEM, "CreateThread", StrError(GetLastError()), __FILE__, __LINE__);
        }
    }
    /* 既に読み込んだデータが存在する場合(キャンセル時) */
    else if (io->offset > 0) {
        if (!SetEvent(io->ctlblk.hEvent)) {
            perr(PERR_SYSTEM, "SetEvent", StrError(GetLastError()), __FILE__, __LINE__);
            return;
        }
    }
    if (io->mlink.tqe_prev == NULL) {
        TAILQ_INSERT_TAIL(&__prc__mioq, io, mlink);
    }
}

/**
 * 標準コンソール出力IOの処理
 */
static void io_stdout(ioent_t *io, event_t *evt, int exec) {
//    printf("io_stdout: enter(exec=%d)\n", exec);
//    fflush(stdout);

    if (exec) {
        if (!WriteFile(io->handle, STRPTR(evt->val), STRLEN(evt->val), (LPDWORD)&io->offset, NULL)) {
            perr(PERR_SYSTEM, "WriteFile", StrError(GetLastError()), __FILE__, __LINE__);
            return;
        }
        io_write_complete(io, io->offset);
        /* IO待ちプロセスが無ければ終了 */
        if ((evt = chout_next(io->chan)) == NULL) {
            return;
        }
        io->iof(io, evt, 0);
        return;
    } 

    if (!SetEvent(io->ctlblk.hEvent)) {
        perr(PERR_SYSTEM, "SetEvent", StrError(GetLastError()), __FILE__, __LINE__);
    }
    if (io->mlink.tqe_prev == NULL) {
        TAILQ_INSERT_TAIL(&__prc__mioq, io, mlink);
    }
}

static void io_tmout(ioent_t *io, event_t *evt, int exec) {
    TAILQ_REMOVE(&io->chan->outq, evt, link);
    timer_add(evt);
}

/* ディスクリプタ集合を初期化する */
static int io_set_events(HANDLE *events, ioent_t **ioents) {
    ioent_t *io;
    int count = 0;

    for (io = __prc__mioq.tqh_first; io != NULL; io = io->mlink.tqe_next) {
	if ((io->iotype == IOT_INPUT && chin_next(io->chan) == NULL) ||
            (io->iotype == IOT_OUTPUT && chout_next(io->chan) == NULL)) {
            /* キャンセルされた場合 */
            TAILQ_REMOVE(&__prc__mioq, io, mlink);
            io->mlink.tqe_prev = NULL;
            continue;
        }
        ioents[count] = io;
	events[count] = io->ctlblk.hEvent;
        count++;
        if (count >= MAXIMUM_WAIT_OBJECTS) {
            perr(PWRN_DESCLIM);
            break;
        }
    }

    return count;
}

/**
 * I/O入出力処理 
 */
int io_exec(void) {
    static HANDLE events[MAXIMUM_WAIT_OBJECTS];
    static ioent_t *ioents[MAXIMUM_WAIT_OBJECTS];
    event_t *evt;
    DWORD timo;
    DWORD ret;
    int n;

    /* 多重化IOのイベントリストを初期化 */
    n = io_set_events(events, ioents);
    /* タイマーイベントを取得 */
    timo = timer_next();
    if (n == 0) {
        if (timo == INFINITE && aio_count == 0) {
            return (int)__stop__;
        }
        if ((ret = SleepEx(timo, TRUE)) == 0) {
            /* タイムアウト時 */
            evt = timer_take();
            EV_SET_CANCEL(evt);
            __prc__regs[0] = evt->clos;
            __prc__regs[1] = evt->val;
            return ((int *)evt->clos)[0];
        }
        
        return (int)__disp__;
    }
    ret = WaitForMultipleObjectsEx(n, events, FALSE, timo, TRUE);
    if (ret >= WAIT_OBJECT_0 && ret - WAIT_OBJECT_0 < n) {
        ioent_t *io = ioents[ret - WAIT_OBJECT_0];
        TAILQ_REMOVE(&__prc__mioq, io, mlink);
        io->mlink.tqe_prev = NULL;
        /* 多重IOの処理 */
        if ((io->iotype == IOT_INPUT && (evt = chin_next(io->chan)) != NULL) ||
            (io->iotype == IOT_OUTPUT && (evt = chout_next(io->chan)) != NULL)) {
            EV_SET_CANCEL(evt);
            evt->trans = 0;
            io->iof(io, evt, !0);
        } else if (io->iotype == IOT_EVENT) {
            io->iof(io, NULL, 0);
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
 * 非同期IOの完了を待つ
 */
static int cs_count;
static HANDLE cs_event;
void io_wait_cs(void) {
    while (WAIT_OBJECT_0 != WaitForSingleObjectEx(cs_event, INFINITE, TRUE));
}
void io_enter_cs(void) {
    if (cs_count++ == 0) {
        if (!ResetEvent(cs_event)) {
            perr(PERR_SYSTEM, "ResetEvent", StrError(GetLastError()), __FILE__, __LINE__);
            return;
        }
    }
}
void io_leave_cs(void) {
    if (--cs_count == 0) {
        if (!SetEvent(cs_event)) {
            perr(PERR_SYSTEM, "SetEvent", StrError(GetLastError()), __FILE__, __LINE__);
            return;
        }
    }
}

/**
 * I/O処理の初期化
 */
void io_init(void) {
    TAILQ_INIT(&__prc__ioq);
    TAILQ_INIT(&__prc__mioq);

    /* I/Oチャネルの登録 */
    __prc__stdout = (int)__chan__();
    __prc__stdin  = (int)__chan__();
    __prc__timer  = (int)__chan__();

    ioent_create((chan_t *)__prc__stdout, GetStdHandle(STD_OUTPUT_HANDLE), IOT_OUTPUT, io_stdout, BUFSIZ);
    ioent_create((chan_t *)__prc__stdin, GetStdHandle(STD_INPUT_HANDLE), IOT_INPUT, io_stdin, BUFSIZ);
    ioent_create((chan_t *)__prc__timer, 0, IOT_OUTPUT, io_tmout, 0);

    /* 非同期I/O保護用の通知イベントを初期化 */
    cs_event = CreateEvent(NULL, TRUE, TRUE, NULL);
    if (cs_event == NULL) {
        perr(PERR_SYSTEM, "CreateEvent", StrError(GetLastError()), __FILE__, __LINE__);
        return;
    }
}

/**
 * IOエントリの新規作成
 */
ioent_t *ioent_create(chan_t *ch, HANDLE handle, iotype_t iotype, iof_t iof, size_t size) {
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

    memset(&io->ctlblk, 0, sizeof(OVERLAPPED));
    /* 自動リセットイベントを作成 */
    if ((io->ctlblk.hEvent = CreateEvent(NULL, FALSE, FALSE, NULL)) == NULL) {
	perr(PERR_SYSTEM, "CreateEvent", StrError(GetLastError()), __FILE__, __LINE__);
        return NULL;
    }

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

