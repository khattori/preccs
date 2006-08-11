/**
 * @file 
 * @brief �v���Z�X���s�G���W��(���s�����C�u����)
 *
 * @author Kenta HATTORI
 * @date   2006/04/18
 * $Id: exec.c,v 1.6 2006/07/27 00:06:56 hattori Exp $
 */
#include <stdlib.h>
#include <stdio.h>
#include <windows.h>
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
 * �v���Z�X�f�B�X�p�b�`��
 */
int __disp__(void) {
    proc_t *np;

    /* ���s�҂��v���Z�X���������I/O���� */
    if ((np = __prc__rdyq->tqh_first) == NULL) {
        return (int)io_exec;
    }

    /* ���s�҂��L���[����폜 */
    TAILQ_REMOVE(__prc__rdyq, np, link);

    /* �N���[�W���K�p */
    __prc__regs[0] = np->clos;
    __prc__regs[1] = np->val;   /* �����͈�� */

    return ((int *)__prc__regs[0])[0];
}

static int send_clos[1] = { (int)__send__ };
static int recv_clos[1] = { (int)__recv__ };
static int run_clos[1]  = { (int)__run__ };
int __prc__send = (int)send_clos;
int __prc__recv = (int)recv_clos;
int __prc__run  = (int)run_clos;

/*
 * �`���l�����M�F�C�x���g�ƃA�N�V�������`���l���ɓo�^
 */
/*
  r0 : send_clos
  r1 : continuation
  r2 : channerl
  r3 : val
  r4 : clos
  r5 : trans
*/
int __send__(void) {
    if ((__prc__regs[0] = (int)chin_next((chan_t *)__prc__regs[2])) == (int)NULL) {
        __prc__regs[0] = (int)event(__prc__regs[3], __prc__regs[4], __prc__regs[5]);
        TAILQ_INSERT_TAIL(&((chan_t *)__prc__regs[2])->outq, (event_t *)__prc__regs[0], link);
    } else {
        proc_t *prc;

        /* ��M�v���Z�X�����o���Ď��s�҂��L���[�ɓ���� */
        prc = proc();
        prc->clos = ((event_t *)__prc__regs[0])->clos;
        prc->val  = __prc__regs[3];       /* �l�̎󂯓n�� */
        TAILQ_INSERT_TAIL(__prc__rdyq, prc, link);

        prc = proc();
        prc->clos = __prc__regs[4];
        prc->val  = 0;
        TAILQ_INSERT_TAIL(__prc__rdyq, prc, link);

        TAILQ_REMOVE(&((chan_t *)__prc__regs[2])->inq, (event_t *)__prc__regs[0], link);
        EV_SET_CANCEL((event_t *)__prc__regs[0]);
        /* trans���L�����Z������(__prc__regs[5]) */
        TR_SET_CANCEL(__prc__regs[5]);
    }
    __prc__regs[0] = __prc__regs[1];
    return ((int*)__prc__regs[0])[0];
}

/*
 * �`���l����M�F�C�x���g�ƃA�N�V�������`���l���ɓo�^
 */
/*
  r0 : recv_clos
  r1 : continuation
  r2 : channerl
  r3 : clos
  r4 : trans
*/
int __recv__(void) {
    proc_t *prc;

    if ((__prc__regs[0] = (int)chout_next((chan_t *)__prc__regs[2])) == (int)NULL) {
        /* ���M�v���Z�X�������ꍇ�̓u���b�N�L���[�ɓ���� */
        __prc__regs[0] = (int)event(0, __prc__regs[3], __prc__regs[4]);
        TAILQ_INSERT_TAIL(&((chan_t *)__prc__regs[2])->inq, (event_t *)__prc__regs[0], link);
    } else {
        /* ���M�v���Z�X�����o���Ď��s�҂��L���[�ɓ���� */
        prc = proc();
        prc->clos = ((event_t *)__prc__regs[0])->clos;
        prc->val  = 0;
        TAILQ_INSERT_TAIL(__prc__rdyq, prc, link);

        prc = proc();
        prc->clos = __prc__regs[3];
        prc->val  = ((event_t *)__prc__regs[0])->val;    /* �l�̎󂯓n�� */
        TAILQ_INSERT_TAIL(__prc__rdyq, prc, link);

        TAILQ_REMOVE(&((chan_t *)__prc__regs[2])->outq, (event_t *)__prc__regs[0], link);
        EV_SET_CANCEL((event_t *)__prc__regs[0]);
        /* trans���L�����Z������(__prc__regs[4]) */
        TR_SET_CANCEL(__prc__regs[4]);
    }
    __prc__regs[0] = __prc__regs[1];
    return ((int*)__prc__regs[0])[0];
}

/*
  r0 : run_clos
  r1 : continuation
  r2 : ��������v���Z�X�N���[�W��
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
 * �v���Z�X����
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
