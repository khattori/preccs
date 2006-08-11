/**
 * @file 
 * @brief �v���Z�X�Ǘ����W���[��(���s�����C�u����)
 *
 * @author Kenta HATTORI
 * @date   2006/04/18
 * $Id: proc.h,v 1.1 2006/06/21 00:13:41 hattori Exp $
 */
#ifndef __INC_PROC_H__
#define __INC_PROC_H__

#include "queue.h"
#include "gc.h"

/*
 * �v���Z�X�L���[�̃G���g���\���̂̒�`
 */
typedef struct proc_ {
    int clos;                /* �N���[�W�� */
    int val;                 /* ���� */
    TAILQ_ENTRY(proc_) link; /* �����N */
} proc_t;
typedef TAILQ_HEAD(rdyq_, proc_) rdyq_t;
extern rdyq_t *__prc__rdyq;

#define proc() ((proc_t *)gc_record(GC_ALIGN(sizeof(proc_t))))
void proc_init(void);

#endif /* __INC_PROC_H__ */
