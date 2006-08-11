/**
 * @file 
 * @brief チャネル管理モジュール(実行時ライブラリ)
 *
 * @author Kenta HATTORI
 * @date   2006/04/18
 * $Id: chan.c,v 1.5 2006/07/27 00:06:56 hattori Exp $
 */
#include <stdlib.h>
#include "prcrt.h"
#include "exec.h"
#include "perr.h"
#include "event.h"
#include "chan.h"
#include "gc.h"

/**
 * チャネル生成
 */
chan_t *__chan__(void) {
    chan_t *ch;

    ch = (chan_t *)gc_record(GC_ALIGN(sizeof(chan_t)));
    /* イベントキュー初期化 */
    TAILQ_INIT(&ch->inq);
    TAILQ_INIT(&ch->outq);

    return ch;
}

/**
 * チャネルの入力待ちキューから次のイベントを取得
 */
event_t *chin_next(chan_t *ch) {
    event_t *evt;

    while ((evt = ch->inq.tqh_first) != NULL) {
        if (!EV_IS_CANCELLED(evt)) {
            break;
        }
        /* キャンセル済みのイベント */
        TAILQ_REMOVE(&ch->inq, evt, link);
    }

    return evt;
}

/**
 * チャネルの出力待ちキューから次のイベントを取得
 */
event_t *chout_next(chan_t *ch) {
    event_t *evt;

    for (evt = ch->outq.tqh_first; evt != NULL; evt = evt->link.tqe_next) {
        if (!EV_IS_CANCELLED(evt)) {
            break;
        }
        /* キャンセル済みのイベント */
        TAILQ_REMOVE(&ch->outq, evt, link);
    }

    return evt;
}

/**
 * チャネルの送信処理(受信待ちプロセスのキャンセル)
 */
//int chan_send(int ch, char *buf, int len) {
int chan_send(int ch, int val) {
    __prc__regs[0] = __prc__send;
    __prc__regs[1] = __prc__disp;
    __prc__regs[2] = ch;
    __prc__regs[3] = val;
    __prc__regs[4] = __prc__disp;
    __prc__regs[5] = __record__(1,0);

    return (int)__send__;
}

/**
 * チャネルの受信処理(送信待ちプロセスのキャンセル)
 */
int chan_recv(int ch) {
    __prc__regs[0] = __prc__recv;
    __prc__regs[1] = __prc__disp;
    __prc__regs[2] = ch;
    __prc__regs[3] = __prc__disp;
    __prc__regs[4] = __record__(1,0);

    return (int)__recv__;
}

