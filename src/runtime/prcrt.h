/**
 * @file 
 * @brief 実行時ライブラリ 共通ヘッダファイル
 *
 * @author Kenta HATTORI
 * @date   2006/04/18
 * $Id$
 */
#ifndef __INC_PRCRT_H__
#define __INC_PRCRT_H__

#ifdef WIN32
#include <windows.h>
#endif
#include <string.h>
#include <assert.h>
#include "chan.h"
#include "dfa.h"
#include "gc.h"
#include "io.h"
#include "sock.h"
#include "file.h"
#include "wave.h"

/* Preccsメイン関数 */
int prc_main(void);	/* コンパイラが生成する関数 */

typedef int (*prc_func_t)(void);
int __prc__main__(int rnum, prc_func_t init, prc_dtable_t *dtbl);
void __prc__set_heapsz(int size);

/* グローバルチャネルの宣言 */
extern int __prc__stdout;
extern int __prc__stdin;
extern int __prc__timer;
extern int __prc__cond;
extern int __prc__null;

/* レジスタ宣言 */
extern int __prc__treg;
extern int *__prc__regs;
extern int __prc__rnum;

extern int __prc__disp;
extern int __prc__send;
extern int __prc__recv;

extern int __prc__temp;
extern int __prc__temp1;
extern int __prc__temp2;
extern int __prc__temp3;

/* 組込み関数 */
int __run__(int s);
int __pullup__(int s);
int __strlen__(int s);
char *__strptr__(int s);
int __concat__(int s1, int s2);
int __equals__(int s1, int s2);
int __disp__(void);
int __stop__(void);
int __record__(int sz);
int __rexrcd__(int s, int r);
int __string__(int len, char *buf);
int __salloc__(int len);
int __dmatch__(int val, u_int st);

#define STRPTR(s) __strptr__(s)
#define STRLEN(s) __strlen__(s)
#define RCDIDX(s,i) (((int*)s)[i])

#define IS_SSTR(s) (((int*)s)[3]&0x1)
/* 文字列データの単体ノード */
#define SSTRLEN(s) ((((int*)s)[3]-((int*)s)[0])>>1)
#define SSTRPTR(s) ((char*)((int*)s)[1]+(((int*)s)[0]>>1))
/* 文字列データの中間ノード */
#define NSTRLEN(s) ((((int*)s)[2]-((int*)s)[0])>>1)
#define NSTRPTR(s) ((char*)((int*)s)[1]+(((int*)s)[0]>>1))
#define NSTRNXT(s) (((int*)s)[3])

#define TOCINT(i) ((i)>>1)
#define TOPINT(i) (((i)<<1)^0x01)

#define IADD(a,b) ((a)+(b)-1)
#define ISUB(a,b) ((a)-(b)+1)
#define IMUL(a,b) ((((a/2)*(b/2))<<1)^1)
#define IDIV(a,b) ((((a/2)/(b/2))<<1)^1)
#define IMOD(a,b) ((((a/2)%(b/2))<<1)^1)
#define EQ(a,b)  ((a)==(b)?~0:1)
#define NEQ(a,b) ((a)!=(b)?~0:1)
#define LT(a,b)  ((a)<(b)?~0:1)
#define LEQ(a,b) ((a)<=(b)?~0:1)
#define GT(a,b)  ((a)>(b)?~0:1)
#define GEQ(a,b) ((a)>=(b)?~0:1)
#define AND(a,b) ((a)&(b))
#define OR(a,b)  ((a)|(b))
#define EQS(a,b)  (__prc__temp1=a,__prc__temp2=b,__equals__(__prc__temp1,__prc__temp2)?~0:1)
#define CONCAT(a,b) (__prc__temp1=a,__prc__temp2=b,__concat__(__prc__temp1,__prc__temp2))

#define INEG(a)  TOPINT(-TOCINT(a))
#define NOT(a)  (-(a))
#define SET(a)  (a)

void validate(void);

#endif /* __INC_PRCRT_H__ */
