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
#include <string.h>

#include "dfa.h"
#include "gc.h"
#include "prcrt.h"

static int con_field(char *con);
static u_char *origin;

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
    state_t *state = &__prc__dtable.state[TOCINT(st)];
    act_t act = state->nact;
    u_int idx = state->nidx;
    u_char *p, *ep;
    int i;
    int tmp;
    int *retval;

    __prc__temp = val;
    __prc__temp1 = (int)NULL;
    __prc__temp2 = (int)NULL;
    /* ラベル記録ポインタの初期化 */
    for (i = 0; i < __prc__dtable.lbl_max; i++) {
        __prc__lbl_ptr[i] = NULL;
    }
    origin = (u_char *)((int*)val)[1];
    p = STRPTR(val);              /* データ   */
    ep     = STRPTR(val)+STRLEN(val);  /* 終了位置 */

    for (;;) switch (act) {
    case ACT_MATCH: { /* 文字列マッチ */
        int n = *p >> 5;
        u_int b = 0x1 << (*p % 32);
        mact_t *mact;
        u_int *cset;

        do {
            mact = &__prc__dtable.mact[idx];
            cset = __prc__dtable.cset[mact->cidx];
            idx = mact->midx;
        } while (!(cset[n] & b));
        act = mact->nact;
        idx = mact->nidx;
        p++;          /* 文字ポインタの位置を進める */
        break;
    }
    case ACT_SKIP: { /* 文字スキップ */
        mact_t *mact = &__prc__dtable.mact[idx];
        act = mact->nact;
        idx = mact->nidx;
        p++;          /* 文字ポインタの位置を進める */
        break;
    }
    case ACT_TRANS: { /* 遷移処理 */
        state = &__prc__dtable.state[idx];
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
        fact_t *fact = &__prc__dtable.fact[idx];
        assert(idx >= 0);

	__prc__temp1 = __record__(2);
	retval = (int*)__prc__temp1;
	retval[0] = TOPINT(fact->sel);
	retval[1] = 0;
        if (fact->cons) {
	    tmp = con_field(fact->cons);
	    retval = (int*)__prc__temp1;
            retval[1] = tmp;
        } else {
            retval[1] = __prc__temp;
        }
	retval = (int*)__prc__temp1;
	__prc__temp  = (int)NULL;
	__prc__temp1 = (int)NULL;
	__prc__temp2 = (int)NULL;
        return (int)retval;
    }
    case ACT_RECORD: { /* 記録処理（ラベルレジスタにオフセット値を記録する）*/
        ract_t *ract = &__prc__dtable.ract[idx];
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
        cond_t *cond = &__prc__dtable.cond[idx];
        u_char lid = cond->lid;
        //__prc__lbl_ptr[lid] = NULL;
        if (cond->func) {
            COND_ACT(cond->func(__prc__lbl_value[lid]) == 0,cond)
        } else {
            COND_ACT(__prc__lbl_value[lid] == 0,cond)
        }
        break;
    }
    case ACT_COND_VALNONZ: {
        cond_t *cond = &__prc__dtable.cond[idx];
        u_char lid = cond->lid;
        //__prc__lbl_ptr[lid] = NULL;
        if (cond->func) {
            COND_ACT(cond->func(__prc__lbl_value[lid]) != 0,cond)
        } else {
            COND_ACT(__prc__lbl_value[lid] != 0,cond)
        }
        break;
    }
    case ACT_COND_CNTZERO: {
        cond_t *cond = &__prc__dtable.cond[idx];
        u_char lid = cond->lid;

        COND_ACT(__prc__lbl_count[lid] == 0,cond)

        break;
    }
    case ACT_COND_CNTNONZ: {
        cond_t *cond = &__prc__dtable.cond[idx];
        u_char lid = cond->lid;

        COND_ACT(__prc__lbl_count[lid] != 0,cond)

        break;
    }
    case ACT_COUNT_SET: {
        cact_t *cact = &__prc__dtable.cact[idx];
        u_char lid = cact->lid;
        u_char lid2 = cact->lid2;

        if (cact->func) {
            __prc__lbl_count[lid] = cact->func(__prc__lbl_value[lid2]) - 1;
        } else {
            __prc__lbl_count[lid] = __prc__lbl_value[lid2] - 1;
        }
        act = cact->nact;
        idx = cact->nidx;
        break;
    }
    case ACT_COUNT_DECR: {
        cact_t *cact = &__prc__dtable.cact[idx];
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
#define STACK_SIZE 256
static int con_field(char *con) {
    int *stack;
    static int cstack[256];
    static int lstack[256];

    char *p = con;
    int top, osp;
    int sz;
    int lid;
    int i;
    int tmp;

    __prc__temp2 = __record__(STACK_SIZE);
    memset((int*)__prc__temp2, 0, STACK_SIZE*sizeof(int));
    stack = (int*)__prc__temp2;
    top = 0;
    /* 文字列構造の中身を書き換えるので，コピーが必要 */
    tmp = __record__(4);
    stack = (int*)__prc__temp2;
    stack[top] = tmp;
    ((int*)stack[top])[0] = ((int*)__prc__temp)[0];
    ((int*)stack[top])[1] = ((int*)__prc__temp)[1];
    ((int*)stack[top])[2] = ((int*)__prc__temp)[2];
    ((int*)stack[top])[3] = ((int*)__prc__temp)[3];
    cstack[top] = 1;
    lstack[top] = 1;
    while (*p) switch (*p++) {
    case 'C':
        sz = ((u_char)*p++);
        osp = top++;

        tmp = __record__(sz*3+1);
	memset((int*)tmp, 0, (sz*3+1)*sizeof(int));
  	stack = (int*)__prc__temp2;
        stack[top] = tmp;
        cstack[top] = 0;
        lstack[top] = sz;

        ((int*)stack[osp])[cstack[osp]*3-1] = stack[top];
        for (i = 0; i < lstack[top]; i++) {
            ((int*)stack[top])[i*3+1] = ((int*)__prc__temp)[1];
            ((int*)stack[top])[i*3+2] = 0;
        }
        break;
    case 'S':
        lid = ((u_char)*p++);
        /* フィールドの末尾まで到達した場合 */
        if (cstack[top] == lstack[top]) {
            /* エンドポイントの設定 */
	    assert(__prc__lbl_ptr[lid] != NULL);
            ((int*)stack[top])[cstack[top]*3] = TOPINT((int)__prc__lbl_ptr[lid]-(int)origin);
	    top--;
        }
	assert(__prc__lbl_ptr[lid] != NULL);
        ((int*)stack[top])[cstack[top]++*3] = TOPINT((int)__prc__lbl_ptr[lid]-(int)origin);

        break;
    default: assert(0);
    }
    /* フィールドの末尾の処理 */
    while (top > 0) {
        assert(cstack[top] == lstack[top]);
        ((int*)stack[top])[cstack[top]*3] = ((int*)__prc__temp)[3];
	top--;
    }
    return stack[0];
}
