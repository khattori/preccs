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
#include "timer.h"
#include "perr.h"
#include "sock.h"
#include "wave.h"
#include "exec.h"
#include "io.h"

static int func_print(void) {
    struct aiocb aiocb;
    struct aiocb *aiocbs[1];
    int len;

    bzero((char *)&aiocb, sizeof(aiocb));

    aiocb.aio_fildes = STDOUT_FILENO;
    aiocb.aio_nbytes = STRLEN(__prc__regs[1]);
    aiocb.aio_offset = 0;
    aiocb.aio_buf    = STRPTR(__prc__regs[1]);

    if (!WriteFile(hStdOut, (LPCVOID)STRPTR(__prc__regs[1]), len, &len, NULL)) {
        perr(PERR_SYSTEM, "WriteHandle", GetLastError(), __FILE__, __LINE__);
    }

    return (int)__disp__;
}
static int clos_print[1] = { (int)func_print };
static int func_stdout(void) {
    __prc__regs[0] = __prc__recv;
    __prc__regs[1] = __prc__stdout;
    __prc__regs[2] = (int)clos_print;
    return (int)__recv__;
}

static int func_stdin(void) {
    __prc__regs[0] = __prc__send;
    __prc__regs[1] = __prc__stdin;
    __prc__regs[2] = __string__(dwInbLen, achInbuf);
    __prc__regs[3] = __prc__disp;
    /* バッファ空状態を通知 */
    if (!SetEvent(hEvtInbEmpty)) {
        perr(PERR_SYSTEM, "SetEvent", GetLastError(), __FILE__, __LINE__);
    }

    return (int)__send__;
}
static int clos_stdin[1] = { (int)func_stdin };

/**
 * I/O処理の初期化
 */
void io_init(void) {
    /* ファイルIO初期化 */
    file_init();
}
static HANDLE handles[MAXIMUM_WAIT_OBJECTS];
ioent_t io_table[MAXIMUM_WAIT_OBJECTS];
static int io_async_count;
static int io_multi_count;

/**
 * I/O入出力処理 
 */
int io_exec(void) {
    event_t *evt;
    ioent_t *io;
    int cont;
    int timo;
    int ret;

    timo = timer_next();
    if (io_count == 0) {
        if (timo < 0) {
            return (int)__stop__;
        } else {
            sleep(timo);
            evt = timer_take();
            __prc__regs[0] = evt->clos;
            EV_SET_CANCEL(evt);
        }
    } else {
        ret = WaitForMultipleObjects(io_count, handles, FALSE, timo);
        if (ret < io_count) {
            switch (io_table[ret].type) {
            case IOT_STDIN:
                __prc__regs[0] = (int)clos_stdin;
                break;
            case IOT_SOCK:
                __prc__regs[1] = TOPINT(ret);
                __prc__regs[0] = sock_clos(io_table[ret].handle);
                break;
            case IOT_FILE:
                return file_clos(io_table[ret].handle);
            case IOT_WAVE:
                return (int)__disp__;
            default:
                perr(PERR_INTERNAL, __FILE__, __LINE__);
            }
        } else if (ret == WAIT_TIMEOUT) {
            evt = timer_take();
            __prc__regs[0] = evt->clos;
            EV_SET_CANCEL(evt);
        } else {
            perr(PERR_SYSTEM, "WaitForMultipleObjects", ret, __FILE__, __LINE__);
        }
    }

    return ((int *)__prc__regs[0])[0];
}

/* 標準入力の監視スレッド */
static DWORD WINAPI StdinThread(LPVOID param) {
    /* バッファが空になるまで待つ */
    while (WaitForSingleObject(hEvtInbEmpty, INFINITE) == WAIT_OBJECT_0) {
        ReadFile(hStdIn, achInbuf, BUFSIZ, &dwInbLen, NULL);
        /* バッファをセットする */
        if (!SetEvent(hEvtInbFull)) {
            perr(PERR_SYSTEM, "SetEvent", GetLastError(), __FILE__, __LINE__);
        }
    }

    return 0;
}
