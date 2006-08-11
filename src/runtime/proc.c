/**
 * @file 
 * @brief プロセス管理モジュール(実行時ライブラリ)
 *
 * @author Kenta HATTORI
 * @date   2006/04/18
 * $Id: proc.c,v 1.2 2006/07/11 00:12:32 hattori Exp $
 */
#include <stdlib.h>
#include <stdio.h>
#include "proc.h"
rdyq_t *__prc__rdyq;

/**
 * プロセス処理の初期化
 */
void proc_init(void) {
    /* 実行待ちキューの初期化 */
    __prc__rdyq = (rdyq_t *)gc_record(GC_ALIGN(sizeof(*__prc__rdyq)));
    TAILQ_INIT(__prc__rdyq);
}
void dump_rdyq(void) {
    proc_t *p;
    int i = 0;

    printf("<%p,%p>\n",
           &__prc__rdyq->tqh_first->link.tqe_next,
           __prc__rdyq->tqh_last);
    for (p = __prc__rdyq->tqh_first; p != NULL; p = p->link.tqe_next) {
        printf("\t(%p<-)<%p>func=%p:\n",
               p->link.tqe_prev, &p->link.tqe_next, (int*)((int*)p->clos)[0]);
        i++;
    }
    printf("%d procs\n", i);
    fflush(stdout);
}
