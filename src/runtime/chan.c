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
 * 組み込みチャネル初期化
 */
void chan_init(void) {
    __prc__cond = (int)__chan__();
    ((chan_t *)__prc__cond)->sendf = __cond_send__;
    ((chan_t *)__prc__cond)->recvf = NULL;
    __prc__null = (int)__chan__();
    ((chan_t *)__prc__null)->sendf = __null_send__;
    ((chan_t *)__prc__null)->recvf = __null_recv__;
}

/**
 * チャネル生成
 */
chan_t *__chan__(void) {
    chan_t *ch;

    ch = (chan_t *)gc_record(GC_ALIGN(sizeof(chan_t)));
    /* イベントキュー初期化 */
    TAILQ_INIT(&ch->inq);
    TAILQ_INIT(&ch->outq);
    ch->ioent = NULL;
    ch->recvf = __recv__;
    ch->sendf = __send__;

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

    while ((evt = ch->outq.tqh_first) != NULL) {
        if (!EV_IS_CANCELLED(evt)) {
            break;
        }
        /* キャンセル済みのイベント */
        TAILQ_REMOVE(&ch->outq, evt, link);
    }

    return evt;
}



