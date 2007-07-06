/**
 * @file 
 * @brief メモリ管理モジュール(実行時ライブラリ)
 *
 * @author Kenta HATTORI
 * @date   2006/04/18
 * $Id: alloc.c,v 1.2 2006/06/21 00:13:06 hattori Exp $
 */
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "gc.h"
#include "prcrt.h"
#include "perr.h"

/**
 * レコード生成と初期化
 */
int __record__(int sz) {
    int *ret;
    int i;
    ret = gc_record(sz);
    for (i = 0; i < sz; i++) {
        ret[i] = 0;
    }
    return (int)ret;
}

/**
 * 正規表現レコードの作製
 */
#define GET_SIZE(p)    (p[-1]>>3)
static void set_rcd(int s, int *rcd) {
    int len = GET_SIZE(rcd);
    int i;

    for (i = 0; i < len-2; i += 3) {
        rcd[i+1] = s;
        if (rcd[i+2] != 0) {
            set_rcd(s, (int *)rcd[i+2]);
        }
    }
}
int __rexrcd__(int s, int r) {
    int *sval = (int *)s;

    set_rcd(sval[1], (int *)r);
    sval[2] = r;

    return s;
}

/**
 * 文字列の初期化
 */
int __string__(int len, char *buf) {
    int ret;

    ret = __salloc__(len);
    memcpy((char*)((int*)ret)[1], buf, len);
    ((char*)((int*)ret)[1])[len] = '\0';

    return ret;
}

/**
 * 文字列のアロケート(非初期化版)
 */
int __salloc__(int len) {
    int ret;

    __prc__temp = gc_array(GC_ALIGN(len+1));
    ret = __record__(4);
    ((int*)ret)[0] = TOPINT(0);
    ((int*)ret)[1] = __prc__temp;
    ((int*)ret)[2] = 0;
    ((int*)ret)[3] = TOPINT(len);
    __prc__temp = (int)NULL;

    return ret;
}

