/**
 * @file 
 * @brief 文字列モジュール(実行時ライブラリ)
 *
 * @author Kenta HATTORI
 * @date   2007/06/28
 * $Id$
 */
#include <stdlib.h>
#include <stdio.h>
#include "prcrt.h"
#include "proc.h"
#include "timer.h"
#include "perr.h"


int __strlen__(int s) {
    if (IS_SSTR(s)) {
	return SSTRLEN(s);
    }
    return NSTRLEN(s) + __strlen__(((int*)s)[3]);
}

char *__strptr__(int s) {
    if (IS_SSTR(s)) {
        return SSTRPTR(s);
    }
    s = __pullup__(s);
    return SSTRPTR(s);
}

/**
 * 連鎖状の文字列データを連続領域にコピーする
 */
int __pullup__(int s) {
    int offset = 0;
    int len;
    int ret;

    if (IS_SSTR(s)) {
	return s;
    }
    len = __strlen__(s);
    ret = __salloc__(len);
    s = (int)gc_forward((int*)s);
    do {
	len = NSTRLEN(s);
	memcpy(SSTRPTR(ret)+offset, NSTRPTR(s), len);
	offset += len;
	s = NSTRNXT(s);
    } while (!IS_SSTR(s));
    memcpy(SSTRPTR(ret)+offset, SSTRPTR(s), SSTRLEN(s));

    return ret;
}

/**
 * 文字列の連結
 */
int __concat__(int s1, int s2) {
    int len1, len2;
    int s;
    int ret;

    len1 = __strlen__(s1);
    len2 = __strlen__(s2);
    /* どちらかが空文字列ならもう一方を返す */
    if (len1 == 0) {
        return s2;
    } else if (len2 == 0) {
        return s1;
    }
    __prc__temp = s2;
    s = ret = __record__(4);
    s1 = (int)gc_forward((int*)s1);
    while (!IS_SSTR(s1)) {
	((int*)s)[0] = ((int*)s1)[0];
	((int*)s)[1] = ((int*)s1)[1];
	((int*)s)[2] = ((int*)s1)[2];
	s = ((int*)s)[3] = __record__(4);
	s1 = (int)gc_forward((int*)s1);
	s1 = NSTRNXT(s1);
    }
    ((int*)s)[0] = ((int*)s1)[0];
    ((int*)s)[1] = ((int*)s1)[1];
    ((int*)s)[2] = ((int*)s1)[3];
    ((int*)s)[3] = __prc__temp;

    return ret;
}

/**
 * 文字列の中身の比較
 */
int __equals__(int s1, int s2) {
    char *p1,*ep1, *p2, *ep2;

    if (IS_SSTR(s1)) {
	p1  = SSTRPTR(s1);
	ep1 = p1 + SSTRLEN(s1);
    } else {
	p1  = NSTRPTR(s1);
	ep1 = p1 + NSTRLEN(s1);
    }
    if (IS_SSTR(s2)) {
	p2  = SSTRPTR(s2);
	ep2 = p2 + SSTRLEN(s2);
    } else {
	p2  = NSTRPTR(s2);
	ep2 = p2 + NSTRLEN(s2);
    }

    for (;;) {
	if (p1 == ep1 && !IS_SSTR(s1)) {
	    s1 = NSTRNXT(s1);
	    p1 = NSTRPTR(s1);
	    ep1 = p1 + NSTRLEN(s1);
	    continue;
	}
	if (p2 == ep2 && !IS_SSTR(s2)) {
	    s2 = NSTRNXT(s2);
	    p2 = NSTRPTR(s2);
	    ep2 = p2 + NSTRLEN(s2);
	    continue;
	}
	if (p1 == ep1 && p2 == ep2) {
	    break;
	}
	if (p1 == ep1 || p2 == ep2) {
	    return 0;
	}
	if (*p1++ != *p2++) {
	    return 0;
	}
    }
    return !0;
}
