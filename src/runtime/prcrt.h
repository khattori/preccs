#ifndef __INC_PRCRT_H__
#define __INC_PRCRT_H__
#include <windows.h>
#include <assert.h>
#include "chan.h"
#include "dfa.h"
#include "gc.h"
#include "sock.h"
#include "file.h"
#include "wave.h"

/* Preccsメイン関数 */
int prc_main(void);
int __prc__stdin__(void);

/* スタートアップルーチン */
extern int __prc__init(void);

/* グローバルチャネルの宣言 */
extern int __prc__stdout;
extern int __prc__stdin;
extern int __prc__timer;

/* レジスタ宣言 */
extern int __prc__treg;
extern int __prc__regs[];
extern int __prc__rnum;

extern int __prc__disp;
extern int __prc__send;
extern int __prc__recv;
extern int __prc__run;

/* 組込み関数 */
int __concat__(int s1, int s2);
int __equals__(int s1, int s2);
int __disp__(void);
int __stop__(void);
int __record__(int sz, ...);
int __rexrcd__(int s, int r);
int __string__(int len, char *buf);
int __dmatch__(int val, u_int st);

#define STRPTR(s) ((char*)((int*)s)[1]+((int*)s)[0])
#define STRLEN(s) (((int*)s)[3]-((int*)s)[0])

#define TOCINT(i) ((i)>>1)
#define TOPINT(i) (((i)<<1)^0x01)

#define DEFAULT_HEAP_SIZE (1024*1024*16)
/* #define DEFAULT_HEAP_SIZE (1024) */

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

#define INEG(a)  TOPINT(-TOCINT(a))
#define NOT(a)  (-(a))
#define SET(a)  (a)

void validate(void);

#endif /* __INC_PRCRT_H__ */
