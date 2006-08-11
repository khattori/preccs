/**
 * @file 
 * @brief $B%?%$%^!<=hM}%b%8%e!<%k(B($B<B9T;~%i%$%V%i%j(B)
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

/* $B%?%$%^!<%-%e!<$NDj5A(B */
typedef TAILQ_HEAD(tmrq_, event_) tmrq_t;
extern tmrq_t *__prc__tmrq;

#endif /* __INC_TIMER_H__ */
