/**
 * @file 
 * @brief タイマー処理モジュール(実行時ライブラリ)
 *
 * @author Kenta HATTORI
 * @date   2006/04/24
 * $Id: timer.c,v 1.2 2006/06/21 00:13:06 hattori Exp $
 */
#include <sys/time.h>
#include <errno.h>
#include "prcrt.h"
#include "queue.h"
#include "event.h"
#include "timer.h"
#include "gc.h"
#include "perr.h"

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
    struct timeval now;

    if (gettimeofday(&now, NULL) < 0) {
        perr(PERR_SYSTEM, "gettimeofday()", errno, __FILE__, __LINE__);
    }
    RCDIDX(evt->val,0) = now.tv_sec + TOCINT(RCDIDX(evt->val,0));
    RCDIDX(evt->val,1) = now.tv_usec + TOCINT(RCDIDX(evt->val,1)) * 1000;

    /* 挿入場所を探索 */
    for (e = __prc__tmrq->tqh_first; e != NULL; e = e->link.tqe_next) {
	if (e->val > evt->val) {
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
 * 次の発火までの時間をsec単位で取得する
 */
const struct timespec *timer_next(void) {
    static struct timespec ts_zero = {0,0};
    static struct timespec ts;
    event_t *evt;
    struct timeval now, fire;

    /* タイマー待ちプロセスが無い場合 */
    if ((evt = tmrq_next()) == NULL) {
        return NULL;
    }

    if (gettimeofday(&now, NULL) < 0) {
        perr(PERR_SYSTEM, "gettimeofday()", errno, __FILE__, __LINE__);
    }
    fire.tv_sec = RCDIDX(evt->val,0);
    fire.tv_usec = RCDIDX(evt->val,1);
    if (timercmp(&now,&fire,>)) {
        return &ts_zero;  /* 既に発火時刻を過ぎた場合 */
    }
    ts.tv_sec  = fire.tv_sec - now.tv_sec;
    ts.tv_nsec = (fire.tv_usec - now.tv_usec)*1000;

    return &ts;
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
