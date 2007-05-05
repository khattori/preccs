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
#include "dfa.h"

int __prc__treg;
int *__prc__regs;
int __prc__rnum;

int __prc__stdout;
int __prc__stdin;
int __prc__timer;
int __prc__cond;
int __prc__null;
int __prc__temp;
int __prc__temp1;
int __prc__temp2;

static int disp_clos[1] = { (int)__disp__ };
int __prc__disp = (int)disp_clos;

/* 遷移テーブル */
prc_dtable_t __prc__dtable;
/* ラベル用レジスタ */
u_char **__prc__lbl_ptr;
u_int *__prc__lbl_value;
u_int *__prc__lbl_count;

/**
 * Preccsメインルーチン
 */
int __prc__main__(int rnum, prc_func_t init, prc_dtable_t *dtbl) {
    prc_func_t start;

    /* レジスタ初期化 */
    if ((__prc__regs = malloc(sizeof(int) * rnum)) == NULL) {
        perr(PERR_OUTOFMEM, __FILE__, __LINE__);
        return -1;
    }
    memset(__prc__regs, 0, sizeof(int)*rnum);

    /* 状態遷移テーブルの初期化 */
    __prc__dtable = *dtbl;
    if ((__prc__lbl_ptr = malloc(sizeof(u_char*) * dtbl->lbl_max)) == NULL) {
        perr(PERR_OUTOFMEM, __FILE__, __LINE__);
        return -1;
    }
    if ((__prc__lbl_value = malloc(sizeof(u_int) * dtbl->lbl_max)) == NULL) {
        perr(PERR_OUTOFMEM, __FILE__, __LINE__);
        return -1;
    }
    if ((__prc__lbl_count = malloc(sizeof(u_int) * dtbl->lbl_max)) == NULL) {
        perr(PERR_OUTOFMEM, __FILE__, __LINE__);
        return -1;
    }

    gc_init();
    chan_init();
    io_init();
    proc_init();
    timer_init();

    /* プロセスループ */
    start = init;
    for (;;) {
        start = (prc_func_t)start();
    }

    free(__prc__regs);

    return 0;
}
