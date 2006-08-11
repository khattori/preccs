/**
 * @file 
 * @brief DFA�G���W��(���s�����C�u����)
 *
 *  DFA�p�^�[���}�b�`���O�̏������s��
 *
 * @author Kenta HATTORI
 * @date   2006/06/07
 * $Id: dfa.h,v 1.1 2006/06/21 00:13:41 hattori Exp $
 */
#ifndef __INC_DFA_H__
#define __INC_DFA_H__

typedef unsigned int  u_int;
typedef unsigned char u_char;

/*
 * �A�N�V�������
 */
typedef enum {
    ACT_NULL,          /* ������ */
    ACT_MATCH,         /* �����W���}�b�`���� */
    ACT_TRANS,         /* ��ԑJ�� */
    ACT_FINAL,         /* �I������ */
    ACT_RECORD,        /* ���x���L�^���� */
    ACT_COND_VALZERO,  /* ��������(V_l==0) */
    ACT_COND_VALNONZ,  /* ��������(V_l!=0) */
    ACT_COND_CNTZERO,  /* ��������(C_l==0) */
    ACT_COND_CNTNONZ,  /* ��������(C_l!=0) */
    ACT_COUNT_SET,     /* �J�E���^����(C_l=V_l-1) */
    ACT_COUNT_DECR     /* �J�E���^����(C_l--) */
} act_t;

/*
 * ��ԃe�[�u��
 */
typedef struct state_ {
    act_t nact;     /* ������                 */
    int   nidx;     /* ���C���f�b�N�X        */
    int   fidx;     /* �I�������C���f�b�N�X */
} state_t;

/*
 * �I�������e�[�u��
 */
typedef struct fact_ {
    int  sel;       /* �I�����Z���N�^ */
    char *cons;     /* �t�B�[���h�g���ăR�[�h */
} fact_t;
/*
 * �����W���}�b�`�����e�[�u��
 */
typedef struct mact_ {
    act_t nact;      /* ������         */
    int   nidx;      /* ���C���f�b�N�X */
    u_int cset[8];   /* �����W��       */
} mact_t;
/*
 * ���x���L�^�����e�[�u��
 */
typedef struct ract_ {
    act_t nact;      /* ������          */
    int   nidx;      /* ���C���f�b�N�X */
    u_char lid;      /* ���x�����ʎq   */
    u_char lsiz;     /* ���x���T�C�Y   */
} ract_t;
/*
 * ���������e�[�u��
 */
typedef struct cond_ {
    act_t  tact;     /* ����������            */
    int    tidx;     /* �������C���f�b�N�X   */
    act_t  fact;     /* �s����������          */
    int    fidx;     /* �s�������C���f�b�N�X */
    u_char lid;      /* ���x�����ʎq         */
} cond_t;
/*
 * �J�E���^�����e�[�u��
 */
typedef struct cact_ {
    act_t  nact;      /* ������          */
    int    nidx;      /* ���C���f�b�N�X */
    u_char lid;       /* ���x�����ʎq   */
} cact_t;

extern state_t __prc__state_table[];
extern fact_t __prc__fact_table[];
extern mact_t __prc__mact_table[];
extern ract_t __prc__ract_table[];
extern cond_t __prc__cond_table[];
extern cact_t __prc__cact_table[];

/* ���x���p���W�X�^ */
extern u_char *__prc__lbl_ptr[];
extern u_int __prc__lbl_value[];
extern u_int __prc__lbl_count[];

#endif /*  __INC_DFA_H__ */
