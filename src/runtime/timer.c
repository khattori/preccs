/**
 * @file 
 * @brief タイマー処理モジュール(実行時ライブラリ)
 *
 * @author Kenta HATTORI
 * @date   2006/04/24
 * $Id: timer.c,v 1.2 2006/06/21 00:13:06 hattori Exp $
 */
#include <windows.h>
#include "queue.h"
#include "event.h"
#include "timer.h"
#include "gc.h"
#include "prcrt.h"

tmrq_t *__prc__tmrq;

/**
 * タイマー処理の初期化
 */
void timer_init(void) {
    __prc__tmrq = (tmrq_t *)gc_record(GC_ALIGN(sizeof(*__prc__tmrq)));
    TAILQ_INIT(__prc__tmrq);
}

/**
 * タイマーイベントを追加する
 */
void timer_add(event_t *evt) {
    event_t *e;
    DWORD now;

    now = GetTickCount();
    evt->val = now + TOCINT(evt->val)*1000;

    /* 挿入場所を探索 */
    for (e = __prc__tmrq->tqh_first; e != NULL; e = e->link.tqe_next) {
	if ((DWORD)e->val > (DWORD)evt->val) {
	    break;
	}
    }
    /* タイマーキューにイベントを挿入 */
    if (e == NULL) {
	TAILQ_INSERT_TAIL(__prc__tmrq, evt, link);
    } else {
	TAILQ_INSERT_BEFORE(e, evt, link);
    }
}

static event_t *tmrq_next(void) {
    event_t *evt;

    while ((evt = __prc__tmrq->tqh_first) != NULL) {
        if (!EV_IS_CANCELLED(evt)) {
            break;
        }
        /* キャンセル済みのイベント */
        TAILQ_REMOVE(__prc__tmrq, evt, link);
        /* evt_free(evt); */
    }

    return evt;
}

/**
 * 次の発火までの時間をmsec単位で取得する
 */
DWORD timer_next(void) {
    event_t *evt;
    DWORD now;

    /* タイマー待ちプロセスが無い場合 */
    if ((evt = tmrq_next()) == NULL) {
        return INFINITE;
    }

    now = GetTickCount();
    if (now > (DWORD)evt->val) {
        return 0;  /* 既に発火時刻を過ぎた場合 */
    }

    return (DWORD)evt->val - now;
}

/**
 * 次のタイマーイベントを取り出す
 */
event_t *timer_take(void) {
    event_t *evt;

    evt = __prc__tmrq->tqh_first;
    TAILQ_REMOVE(__prc__tmrq, evt, link);

    return evt;
}
