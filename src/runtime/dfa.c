/**
 * @file 
 * @brief DFAエンジン(実行時ライブラリ)
 *
 *  DFAパターンマッチングの処理を行う
 *
 * @author Kenta HATTORI
 * @date   2006/06/07
 * $Id: dfa.c,v 1.3 2006/07/13 07:54:00 hattori Exp $
 */
#include <stdlib.h>

#include "dfa.h"
#include "gc.h"
#include "prcrt.h"

static int con_field(char *con, int *val);

#define COND_ACT(cond,ce) \
if (cond) { act = (ce)->tact; idx = (ce)->tidx; } else { act = (ce)->fact; idx = (ce)->fidx; }

/*
 * DFAパタンマッチ関数
 *
 *   引　数：val --- 入力文字列
 *           n   --- DFA初期状態
 *
 *   戻り値：{受理番号,受理データ}の組
 */
int __dmatch__(int val, u_int st) {
    state_t *state = &__prc__state_table[TOCINT(st)];
    act_t act = state->nact;
    u_int idx = state->nidx;
    u_char *p, *ep;

    p      = STRPTR(val);              /* データ   */
    ep     = STRPTR(val)+STRLEN(val);  /* 終了位置 */
    if (p==ep) {
        act = ACT_FINAL;
        idx = state->fidx;
    }

    for (;;) switch (act) {
    case ACT_MATCH: { /* 文字列マッチ */
        int n = *p >> 5;
        u_int b = 0x1 << (*p % 32);

        mact_t *mact = &__prc__mact_table[idx++];
        while (!(mact->cset[n] & b)) {
            mact = &__prc__mact_table[idx++];
        }
        act = mact->nact;
        idx = mact->nidx;
        
        break;
    }
    case ACT_TRANS: { /* 遷移処理 */
        state = &__prc__state_table[idx];
        p++;          /* 文字ポインタの位置を進める */
        if (p==ep) {
            act = ACT_FINAL;
            idx = state->fidx;
        }
        else {
            act = state->nact;
            idx = state->nidx;
        }
        break;
    }
    case ACT_FINAL: { /* 終了処理 */
        fact_t *fact = &__prc__fact_table[idx];
        int *retval;

        retval = (int*)__record__(2, TOPINT(fact->sel), 0);
        val = (int)gc_forward((int*)val);
        if (fact->cons) {
            retval[1] = con_field(fact->cons, (int*)val);
        } else {
            retval[1] = val;
        }

        return (int)retval;
    }
    case ACT_RECORD: { /* 記録処理（ラベルレジスタにオフセット値を記録する）*/
        ract_t *ract = &__prc__ract_table[idx];
        u_char lid = ract->lid;
        int i;

        __prc__lbl_value[lid] = 0;
        __prc__lbl_ptr[lid] = p;
        for (i = 0; i < ract->lsiz; i++) {
            __prc__lbl_value[lid]
                = (__prc__lbl_value[lid]<<8) + __prc__lbl_ptr[lid][i];
        }

        act = ract->nact;
        idx = ract->nidx;
        break;
    } 
    case ACT_COND_VALZERO: { /* 条件マッチ */
        cond_t *cond = &__prc__cond_table[idx];
        u_char lid = cond->lid;

        COND_ACT(__prc__lbl_value[lid] == 0,cond)

        break;
    }
    case ACT_COND_VALNONZ: {
        cond_t *cond = &__prc__cond_table[idx];
        u_char lid = cond->lid;

        COND_ACT(__prc__lbl_value[lid] != 0,cond)

        break;
    }
    case ACT_COND_CNTZERO: {
        cond_t *cond = &__prc__cond_table[idx];
        u_char lid = cond->lid;

        COND_ACT(__prc__lbl_count[lid] == 0,cond)

        break;
    }
    case ACT_COND_CNTNONZ: {
        cond_t *cond = &__prc__cond_table[idx];
        u_char lid = cond->lid;

        COND_ACT(__prc__lbl_count[lid] != 0,cond)

        break;
    }
    case ACT_COUNT_SET: {
        cact_t *cact = &__prc__cact_table[idx];
        u_char lid = cact->lid;

        __prc__lbl_count[lid] = __prc__lbl_value[lid] - 1;
        act = cact->nact;
        idx = cact->nidx;
        break;
    }
    case ACT_COUNT_DECR: {
        cact_t *cact = &__prc__cact_table[idx];
        u_char lid = cact->lid;

        __prc__lbl_count[lid]--;
        act = cact->nact;
        idx = cact->nidx;
        break;
    }

    default: assert(0);
    }

    assert(0);
    return 0;
}

typedef struct stent_ {
    int *field;
    u_char count;
    u_char length;
} stent_t;

static int con_field(char *con, int *val) {
    static stent_t stack[256];
    static int top;

    char *p = con;
    stent_t *st,*ost;
    int sz;
    int lid;
    int i;

    top = 0;
    st = &stack[top];
    /* 文字列構造の中身を書き換えるので，コピーが必要 */
    st->field  = (int*)__record__(4,val[0],val[1],val[2],val[3]);
    val = gc_forward(val);
    
    st->count  = 1;
    st->length = 1;
    while (*p) switch (*p++) {
    case 'C':
        sz = ((u_char)*p++);
        ost = &stack[top];
        st = &stack[++top];

        st->field = (int *)gc_record(sz*3+1);
        val = gc_forward(val);
        for (i = 0; i < top; i++) {
            stack[i].field = gc_forward(stack[i].field);
        }
        st->count = 0;
        st->length = sz;

        ost->field[ost->count*3-1] = (int)st->field;
        for (i = 0; i < st->length; i++) {
            st->field[i*3+1] = val[1];
        }
        break;
    case 'S':
        lid = ((u_char)*p++);
        st = &stack[top];
        /* フィールドの末尾まで到達した場合 */
        if (st->count==st->length) {
            /* エンドポイントの設定 */
            st->field[st->count*3] = (int)__prc__lbl_ptr[lid]-val[1];
            st = &stack[--top];
        }
        st->field[st->count++*3] = (int)__prc__lbl_ptr[lid]-val[1];

        break;
    default: assert(0);
    }
    /* フィールドの末尾の処理 */
    while (top > 0) {
        st = &stack[top--];
        assert(st->count == st->length);
        st->field[st->count*3] = val[3];
    }

    return (int)stack[0].field;
}
