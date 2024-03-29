/**
 * @file 
 * @brief DFAエンジン(実行時ライブラリ)
 *
 *  DFAパターンマッチングの処理を行う
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
 * アクション種別
 */
typedef enum {
    ACT_NULL,          /* 無処理 */
    ACT_MATCH,         /* 文字集合マッチ処理 */
    ACT_SKIP,          /* スキップ処理 */
    ACT_TRANS,         /* 状態遷移 */
    ACT_FINAL,         /* 終了処理 */
    ACT_RECORD,        /* ラベル記録処理 */
    ACT_COND_VALZERO,  /* 条件判定(V_l==0) */
    ACT_COND_VALNONZ,  /* 条件判定(V_l!=0) */
    ACT_COND_CNTZERO,  /* 条件判定(C_l==0) */
    ACT_COND_CNTNONZ,  /* 条件判定(C_l!=0) */
    ACT_COUNT_SET,     /* カウンタ処理(C_l=V_l-1) */
    ACT_COUNT_DECR     /* カウンタ処理(C_l--) */
} act_t;

/*
 * 状態テーブル
 */
typedef struct state_ {
    act_t nact;     /* 次処理                 */
    int   nidx;     /* 次インデックス        */
    int   fidx;     /* 終了処理インデックス */
} state_t;

/*
 * 終了処理テーブル
 */
typedef struct fact_ {
    int  sel;       /* 終了時セレクタ */
    char *cons;     /* フィールド組立てコード */
} fact_t;
/*
 * 文字集合マッチ処理テーブル
 */
typedef struct mact_ {
    act_t nact;      /* 次処理         */
    int   nidx;      /* 次インデックス */
    int   midx;      /* マッチ失敗時 */
    int   cidx;      /* 文字集合インデックス */
} mact_t;
typedef u_int cset_t[8];
typedef int (*vfunc_t)(int);
/*
 * ラベル記録処理テーブル
 */
typedef struct ract_ {
    act_t nact;      /* 次処理          */
    int   nidx;      /* 次インデックス */
    u_char lid;      /* ラベル識別子   */
    u_char lsiz;     /* ラベルサイズ   */
    vfunc_t func;
} ract_t;
/*
 * 条件処理テーブル
 */
typedef struct cond_ {
    act_t  tact;     /* 成立時処理            */
    int    tidx;     /* 成立時インデックス   */
    act_t  fact;     /* 不成立時処理          */
    int    fidx;     /* 不成立時インデックス */
    u_char lid;      /* ラベル識別子         */
    vfunc_t func;
} cond_t;
/*
 * カウンタ処理テーブル
 */
typedef struct cact_ {
    act_t  nact;      /* 次処理          */
    int    nidx;      /* 次インデックス */
    u_char lid;       /* ラベル識別子   */
    u_char lid2;      /* ラベル識別子   */
    vfunc_t func;
} cact_t;

typedef struct prc_dtable_ {
    state_t *state;
    fact_t *fact;
    mact_t *mact;
    cset_t *cset;
    ract_t *ract;
    cond_t *cond;
    cact_t *cact;
    u_int lbl_max;
} prc_dtable_t;

extern prc_dtable_t __prc__dtable;

/* ラベル用レジスタ */
extern u_char **__prc__lbl_ptr;
extern u_int *__prc__lbl_value;
extern u_int *__prc__lbl_count;

#endif /*  __INC_DFA_H__ */
