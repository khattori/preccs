/**
 * @file 
 * @brief プロセス実行エンジン(実行時ライブラリ)
 *
 * @author Kenta HATTORI
 * @date   2006/04/18
 * $Id: prcrt.c,v 1.5 2006/07/13 01:30:17 hattori Exp $
 */
#include "prcrt.h"
#include "perr.h"
#include "io.h"
#include "gc.h"
#include "proc.h"
#include "timer.h"

int __prc__stdout;
int __prc__stdin;
int __prc__timer;

static int disp_clos[1] = { (int)__disp__ };
int __prc__disp = (int)disp_clos;

typedef int (*func_t)(void);

/**
 * Preccsメインルーチン
 */
int prc_main(void) {
    func_t start;

    gc_init();
    io_init();
    proc_init();
    timer_init();

    /* プロセスループ */
    start = __prc__init;
    for (;;) {
        start = (func_t)start();
    }

    return 0;
}
