/**
 * @file 
 * @brief �`���l���Ǘ����W���[��(���s�����C�u����)
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
 * �`���l������
 */
chan_t *__chan__(void) {
    chan_t *ch;

    ch = (chan_t *)gc_record(GC_ALIGN(sizeof(chan_t)));
    /* �C�x���g�L���[������ */
    TAILQ_INIT(&ch->inq);
    TAILQ_INIT(&ch->outq);

    return ch;
}

/**
 * �`���l���̓��͑҂��L���[���玟�̃C�x���g���擾
 */
event_t *chin_next(chan_t *ch) {
    event_t *evt;

    while ((evt = ch->inq.tqh_first) != NULL) {
        if (!EV_IS_CANCELLED(evt)) {
            break;
        }
        /* �L�����Z���ς݂̃C�x���g */
        TAILQ_REMOVE(&ch->inq, evt, link);
    }

    return evt;
}

/**
 * �`���l���̏o�͑҂��L���[���玟�̃C�x���g���擾
 */
event_t *chout_next(chan_t *ch) {
    event_t *evt;

    for (evt = ch->outq.tqh_first; evt != NULL; evt = evt->link.tqe_next) {
        if (!EV_IS_CANCELLED(evt)) {
            break;
        }
        /* �L�����Z���ς݂̃C�x���g */
        TAILQ_REMOVE(&ch->outq, evt, link);
    }

    return evt;
}

/**
 * �`���l���̑��M����(��M�҂��v���Z�X�̃L�����Z��)
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
 * �`���l���̎�M����(���M�҂��v���Z�X�̃L�����Z��)
 */
int chan_recv(int ch) {
    __prc__regs[0] = __prc__recv;
    __prc__regs[1] = __prc__disp;
    __prc__regs[2] = ch;
    __prc__regs[3] = __prc__disp;
    __prc__regs[4] = __record__(1,0);

    return (int)__recv__;
}

