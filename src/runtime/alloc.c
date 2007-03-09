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
    return (int)gc_record(sz);
}

/**
 * 正規表現レコードの作製
 */
#define GET_SIZE(p)    (p[-1]>>3)
static void set_rcd(int s, int *rcd) {
    int len = GET_SIZE(rcd);
    int i;

    for (i = 0; i < len-1; i += 3) {
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
 * 文字列のアロケート
 */
int __string__(int len, char *buf) {
    int ret;

    __prc__temp = gc_array(GC_ALIGN(len+1));
    ret = __record__(4);
    ((int*)ret)[0] = 0;
    ((int*)ret)[1] = __prc__temp;
    ((int*)ret)[2] = 0;
    ((int*)ret)[3] = len;
    memcpy((char*)__prc__temp, buf, len);
    ((char*)__prc__temp)[len] = '\0';
    __prc__temp = (int)NULL;

    return ret;
}

/**
 * 文字列の連結
 */
int __concat__(int s1, int s2) {
    int len1, len2;
    int ret;

    len1 = STRLEN(s1);
    len2 = STRLEN(s2);

    __prc__temp = gc_array(GC_ALIGN(len1+len2+1));
    s1 = (int)gc_forward((int*)s1);
    s2 = (int)gc_forward((int*)s2);
    memcpy((char*)__prc__temp, STRPTR(s1), len1);
    memcpy(((char*)__prc__temp)+len1, STRPTR(s2), len2);
    ((char*)__prc__temp)[len1+len2] = '\0';
    ret = __record__(4);
    ((int*)ret)[0] = 0;
    ((int*)ret)[1] = __prc__temp;
    ((int*)ret)[2] = 0;
    ((int*)ret)[3] = len1+len2;
    __prc__temp = (int)NULL;

    return ret;
}
/**
 * 文字列の中身の比較
 */
int __equals__(int s1, int s2) {
    int len1 = STRLEN(s1);
    int len2 = STRLEN(s2);
    
    return len1==len2 ? memcmp(STRPTR(s1),STRPTR(s2),len1)==0 : 0;
}
