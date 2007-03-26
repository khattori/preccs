/**
 * @file 
 * @brief プロセス実行エンジン(実行時ライブラリ)
 *
 * @author Kenta HATTORI
 * @date   2006/04/18
 * $Id: exec.c,v 1.6 2006/07/27 00:06:56 hattori Exp $
 */
#include <stdlib.h>
#include <stdio.h>
#include "prcrt.h"
#include "perr.h"
#include "queue.h"
#include "event.h"
#include "proc.h"
#include "chan.h"
#include "timer.h"
#include "io.h"
#include "exec.h"

/*
 * プロセスディスパッチャ
 */
int __disp__(void) {
    proc_t *np;

    /* 実行待ちプロセスが無ければI/O処理 */
    if ((np = __prc__rdyq->tqh_first) == NULL) {
        return (int)io_exec;
    }

    /* 実行待ちキューから削除 */
    TAILQ_REMOVE(__prc__rdyq, np, link);

    /* クロージャ適用 */
    __prc__regs[0] = np->clos;
    __prc__regs[1] = np->val;   /* 引数は一つ */

    return ((int *)__prc__regs[0])[0];
}

static int __send0__(void) {
    if (TR_IS_CANCELLED(__prc__regs[5])) {
        return (int)__disp__;
    }
    return (int)((chan_t *)__prc__regs[2])->sendf;
}
static int __recv0__(void) {
    if (TR_IS_CANCELLED(__prc__regs[4])) {
        return (int)__disp__;
    }
    return (int)((chan_t *)__prc__regs[2])->recvf;
}

static int send_clos[1]   = { (int)__send0__ };
static int recv_clos[1]   = { (int)__recv0__ };
static int run_clos[1]    = { (int)__run__ };
int __prc__send   = (int)send_clos;
int __prc__recv   = (int)recv_clos;
int __prc__run    = (int)run_clos;


/*
 * チャネル送信：イベントとアクションをチャネルに登録
 */
/*
  r0 : send_clos
  r1 : continuation
  r2 : channel
  r3 : val
  r4 : clos
  r5 : trans
*/
int __send__(void) {
    if ((__prc__regs[0] = (int)chin_next((chan_t *)__prc__regs[2])) == (int)NULL) {
	ioent_t *io;
	event_t *evt;

	/* キャンセルイベントを削除 */
	(void)chout_next((chan_t *)__prc__regs[2]);
	/* 新規イベント追加 */
        evt = event(__prc__regs[3], __prc__regs[4], __prc__regs[5]);
	if ((io = ((chan_t *)__prc__regs[2])->ioent) != NULL) {
	    io_chout(io, evt);
	} else {
	    TAILQ_INSERT_TAIL(&((chan_t *)__prc__regs[2])->outq, evt, link);
	}
    } else {
        proc_t *prc;

        /* 受信プロセスを取り出して実行待ちキューに入れる */
        prc = proc();
        prc->clos = ((event_t *)__prc__regs[0])->clos;
        prc->val  = __prc__regs[3];       /* 値の受け渡し */
        TAILQ_INSERT_TAIL(__prc__rdyq, prc, link);

        prc = proc();
        prc->clos = __prc__regs[4];
        prc->val  = 0;
        TAILQ_INSERT_TAIL(__prc__rdyq, prc, link);

        TAILQ_REMOVE(&((chan_t *)__prc__regs[2])->inq, (event_t *)__prc__regs[0], link);
        EV_SET_CANCEL((event_t *)__prc__regs[0]);
        /* transをキャンセルする(__prc__regs[5]) */
        if (__prc__regs[5]) TR_SET_CANCEL(__prc__regs[5]);
    }
    __prc__regs[0] = __prc__regs[1];
    return ((int*)__prc__regs[0])[0];
}
/*
 * 組み込みチャネル送信
 */
int __cond_send__(void) {
    if (__prc__regs[3] == ~0) { /* TRUE */
        proc_t *prc;

        prc = proc();
        prc->clos = __prc__regs[4];
        prc->val  = 0;
        TAILQ_INSERT_TAIL(__prc__rdyq, prc, link);

        /* transをキャンセルする(__prc__regs[5]) */
        if (__prc__regs[5]) TR_SET_CANCEL(__prc__regs[5]);
    }
    __prc__regs[0] = __prc__regs[1];
    return ((int*)__prc__regs[0])[0];
}	
int __null_send__(void) {
    proc_t *prc;

    prc = proc();
    prc->clos = __prc__regs[4];
    prc->val  = 0;
    TAILQ_INSERT_TAIL(__prc__rdyq, prc, link);
    /* transをキャンセルする(__prc__regs[5]) */
    if (__prc__regs[5]) TR_SET_CANCEL(__prc__regs[5]);

    __prc__regs[0] = __prc__regs[1];
    return ((int*)__prc__regs[0])[0];
}	

/*
 * チャネル受信：イベントとアクションをチャネルに登録
 */
/*
  r0 : recv_clos
  r1 : continuation
  r2 : channel
  r3 : clos
  r4 : trans
*/
int __recv__(void) {
    if ((__prc__regs[0] = (int)chout_next((chan_t *)__prc__regs[2])) == (int)NULL) {
	ioent_t *io;
	event_t *evt;

	/* キャンセルイベントを削除 */
	(void)chin_next((chan_t *)__prc__regs[2]);
	/* 新規イベント追加 */
        evt = event(0, __prc__regs[3], __prc__regs[4]);
	if ((io = ((chan_t *)__prc__regs[2])->ioent) != NULL) {
	    io_chin(io, evt);
	}
        TAILQ_INSERT_TAIL(&((chan_t *)__prc__regs[2])->inq, evt, link);
    } else {
	proc_t *prc;

        /* 送信プロセスを取り出して実行待ちキューに入れる */
        prc = proc();
        prc->clos = ((event_t *)__prc__regs[0])->clos;
        prc->val  = 0;
        TAILQ_INSERT_TAIL(__prc__rdyq, prc, link);

        prc = proc();
        prc->clos = __prc__regs[3];
        prc->val  = ((event_t *)__prc__regs[0])->val;    /* 値の受け渡し */
        TAILQ_INSERT_TAIL(__prc__rdyq, prc, link);

        TAILQ_REMOVE(&((chan_t *)__prc__regs[2])->outq, (event_t *)__prc__regs[0], link);
        EV_SET_CANCEL((event_t *)__prc__regs[0]);
        /* transをキャンセルする(__prc__regs[4]) */
        if (__prc__regs[4]) TR_SET_CANCEL(__prc__regs[4]);
    }
    __prc__regs[0] = __prc__regs[1];
    return ((int*)__prc__regs[0])[0];
}
/*
 * 組み込みチャネル受信
 */
int __null_recv__(void) {
    proc_t *prc;

    prc = proc();
    prc->clos = __prc__regs[3];
    TAILQ_INSERT_TAIL(__prc__rdyq, prc, link);
    /* transをキャンセルする(__prc__regs[4]) */
    if (__prc__regs[4]) TR_SET_CANCEL(__prc__regs[4]);

    __prc__regs[0] = __prc__regs[1];
    return ((int*)__prc__regs[0])[0];
}

/*
  r0 : run_clos
  r1 : continuation
  r2 : 生成するプロセスクロージャ
*/
int __run__(void) {
    proc_t *prc;

    prc = proc();
    prc->clos = __prc__regs[2];
    prc->val  = 0;
    TAILQ_INSERT_TAIL(__prc__rdyq, prc, link);

    __prc__regs[0] = __prc__regs[1];
    return ((int*)__prc__regs[0])[0];
}

/*
 * プロセス生成
 */
#if 0
void __run__(int clos) {
    proc_t *prc;

    prc = proc();
    prc->clos = clos;
    prc->val  = 0;
    TAILQ_INSERT_TAIL(__prc__rdyq, prc, link);
}
#endif

int __stop__(void) {
    exit(0);
}
