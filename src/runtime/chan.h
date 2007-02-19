/**
 * @file 
 * @brief チャネル管理モジュール(実行時ライブラリ)
 *
 * @author Kenta HATTORI
 * @date   2006/04/18
 * $Id: chan.h,v 1.2 2006/07/27 00:06:56 hattori Exp $
 */

#ifndef __INC_CHAN_H__
#define __INC_CHAN_H__

#include "type.h"
#include "queue.h"
#include "event.h"
#include "io.h"

/*
 * チャネル構造体の定義
 */
struct chan_ {
    TAILQ_HEAD(inq_,  event_) inq;    /* 入力イベントキュー */
    TAILQ_HEAD(outq_, event_) outq;   /* 出力イベントキュー */
    ioent_t *ioent;                   /* I/Oエントリ */
    code_t   recvf;
    code_t   sendf;
};

void chan_init(void);
chan_t *__chan__(void);
event_t *chin_next(chan_t *ch);
event_t *chout_next(chan_t *ch);

#endif /* __INC_CHAN_H__ */
