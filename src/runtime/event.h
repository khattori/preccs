/**
 * @file 
 * @brief イベント構造体定義(実行時ライブラリ)
 *
 *  イベント構造体はチャネルの入出力キューのエントリとなる
 *
 * @author Kenta HATTORI
 * @date   2006/04/18
 * $Id: event.h,v 1.2 2006/06/21 00:13:06 hattori Exp $
 */
#ifndef __INC_EVENT_H__
#define __INC_EVENT_H__

#include <stdlib.h>
#include "queue.h"

/*
 * イベント構造体定義
 */
typedef struct event_ {
    TAILQ_ENTRY(event_) link; /* チャネルリンク */
    int val;                  /* 送信値          */
    int clos;                 /* 継続クロージャ */
    int trans;                /* トランザクション */
} event_t;

event_t *event(int val, int clos, int trans);
/* void evt_free(event_t *evt); */
/* void evt_cancel(event_t *evt); */

#define EV_IS_CANCELLED(evt) (((int*)(evt)->trans)[0] == ~0)
#define EV_SET_CANCEL(evt) (((int*)(evt)->trans)[0] = ~0)
#define TR_SET_CANCEL(tr) (((int*)tr)[0] = ~0)

#endif /* __INC_EVENT_H__ */
