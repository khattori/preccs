/**
 * @file 
 * @brief タイマー処理モジュール(実行時ライブラリ)
 *
 * @author Kenta HATTORI
 * @date   2006/04/24
 * $Id: timer.h,v 1.2 2006/06/21 00:13:06 hattori Exp $
 */
#ifndef __INC_TIMER_H__
#define __INC_TIMER_H__

#include <windows.h>
#include "event.h"

void timer_init(void);
void timer_add(event_t *evt);
DWORD timer_next(void);
event_t *timer_take(void);

/* タイマーキューの定義 */
typedef TAILQ_HEAD(tmrq_, event_) tmrq_t;
extern tmrq_t *__prc__tmrq;

#endif /* __INC_TIMER_H__ */
