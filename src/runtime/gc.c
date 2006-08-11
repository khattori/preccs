/**
 * @file 
 * @brief GCモジュール(実行時ライブラリ)
 *
 * @author Kenta HATTORI
 * @date   2006/04/18
 * $Id: gc.c,v 1.9 2006/08/07 09:07:51 hattori Exp $
 */
#include <stdlib.h>
#include <stdio.h>
#include "gc.h"
#include "proc.h"
#include "timer.h"
#include "prcrt.h"
#include "perr.h"

static int *heap_top;
static int *heap_free;
static int *heap_bottom;

static int *to_space;
static int *from_space;

static int heap_size  = DEFAULT_HEAP_SIZE;
static int space_size = DEFAULT_HEAP_SIZE / 2;

/*

               +----------------+ 
               |                |
               |                |
from_space --> |                |
               +----------------+ <-- heap_top
               |                |
 heap_free --> |                |
               |................|
  to_space --> +----------------+ <-- heap_bottom
*/

#define GET_SIZE(p)    (p[0]>>3)
#define SET_SIZE(p,s)  (p[0]=((s)<<3)^0x2)
#define SET_ARRY(p)    ((p)[0]^=0x4)
#define FWADDR(p) (p[1])
#define IS_VALUE(p) (((int)p)&0x1)
#define IS_NOPTR(p) ((p)<heap_bottom||(p)>=heap_bottom+heap_size)
#define IS_DESC(p)  (((*(p)&0x3)==0x2)&&(*(p)<0x400000))
#define IS_ARRY(p)  (*(p)&0x4)

#define IS_FRWD(p)  ((p)[0]==0x2)
#define SET_FRWD(p) ((p)[0]=0x2)

static void flip(void);
static int *copy(int *p);

/**
 * GCの初期化
 */
void gc_init(void) {
    heap_bottom = malloc(sizeof(int)*heap_size);
    if (heap_bottom == NULL) {
        perr(PERR_OUTOFMEM, __FILE__, __LINE__);
    }
    to_space = heap_bottom;
    heap_top = to_space + space_size;
    from_space = heap_top + 1;
    heap_free = to_space;
}

void __hlimit__(int n) {
    n++;      /* セルサイズを保持する領域 */
    if (heap_free + n > heap_top)
        flip();
    if (heap_free + n > heap_top)
        perr(PERR_OUTOFMEM, __FILE__, __LINE__);
}

/**
 * ヒープからレコード領域を確保
 */
int *gc_record(int n) {
    int *new_cell;

//  assert(n > 0);
    n++;      /* セルサイズを保持する領域 */
    if (heap_free + n > heap_top)
        flip();
    if (heap_free + n > heap_top)
        perr(PERR_OUTOFMEM, __FILE__, __LINE__);

    new_cell = heap_free;
    SET_SIZE(new_cell,n);
    heap_free += n;

    return new_cell + 1;
}

/**
 * ヒープから配列領域を確保
 */
int *gc_array(int n) {
    int *new_cell;

    assert(n > 0);

    n++;      /* セルサイズを保持する領域 */
    if (heap_free + n > heap_top)
        flip();
    if (heap_free + n > heap_top)
        perr(PERR_OUTOFMEM, __FILE__, __LINE__);

    new_cell = heap_free;
    SET_SIZE(new_cell,n);
    SET_ARRY(new_cell);
    heap_free += n;
    
    return new_cell + 1;
}

int *gc_forward(int *p) {
    int *q;

    if (IS_VALUE(p))
        return p;

    if (IS_NOPTR(p))
        return p;

    /* デスクリプタをバックスキャン */
    for (q = p-1; !IS_DESC(q); q--);
    if (IS_FRWD(q))
        return (int *)FWADDR(q)+(p-q);

    return p;
}

extern int sock_used;
extern int ich_array[];
extern int och_array[];
extern int sock_next[];
extern int sock_inited;

extern int wave_och;
extern int wave_ich;

extern int file_used;
extern int file_next[];
extern int file_ch_array[];

static void flip(void) {
    int *scan;
    int *t;
    int i;
    printf("GC started...\n");
    fflush(stdout);
//    validate();
    /* TOとFROMの入れ替え */
    t = from_space;
    from_space = to_space;
    to_space = t;

    heap_top = to_space + space_size;
    scan = heap_free = to_space;

    for (i = 0; i < __prc__rnum; i++) {
        __prc__regs[i] = (int)copy((int*)__prc__regs[i]);
    }
    __prc__rdyq   = (rdyq_t *)copy((int*)__prc__rdyq);
    __prc__tmrq   = (tmrq_t *)copy((int*)__prc__tmrq);
    __prc__stdout = (int)copy((int*)__prc__stdout);
    __prc__stdin  = (int)copy((int*)__prc__stdin);
    __prc__timer  = (int)copy((int*)__prc__timer);

    /* ハンドルの走査 */
    for (i = file_used; i >= 0; i = file_next[i]) {
        file_ch_array[i] = (int)copy((int*)file_ch_array[i]);
    }
    if (sock_inited)
        for (i = sock_used; i >= 0; i = sock_next[i]) {
            ich_array[i] = (int)copy((int*)ich_array[i]);
            och_array[i] = (int)copy((int*)och_array[i]);
        }
    if (wave_och) {
        wave_och = (int)copy((int*)wave_och);
    }
    if (wave_ich) {
        wave_ich = (int)copy((int*)wave_ich);
    }
    while (scan < heap_free) {
        int *p;
        if (!IS_ARRY(scan))
            for (p = scan + 1; p < scan + GET_SIZE(scan); p++) {
                *p = (int)copy((int*)*p);
            }

        fflush(stdout);
        assert(GET_SIZE(scan) != 0);
        scan += GET_SIZE(scan);
    }
//    validate();

    printf("GC finished(%d reclaimed).\n", heap_top - heap_free);
    fflush(stdout);
}

static int *copy(int *p) {
    int *addr;
    int *q;
    int i;

    /* 値ならばそのまま返す */
    if (IS_VALUE(p))
        return p;

    /* 非ポインタならそのまま返す(関数へのポインタなど) */
    if (IS_NOPTR(p))
        return p;

    if (!(p>=from_space && p < from_space + space_size)) {
        assert(0);
    }

    /* デスクリプタをバックスキャン */
    for (q = p-1; !IS_DESC(q); q--);

    if (IS_FRWD(q))
        return (int *)FWADDR(q)+(p-q);

    addr = heap_free;
    /* move(p,heap_free) */
    for (i = 0; i < GET_SIZE(q); i++) {
        heap_free[i] = q[i];
    }
//    printf ("heap_free:%p:%d\n", q,GET_SIZE(q));
    fflush(stdout);
    if (GET_SIZE(q)==1) {
        assert(0);
    }
    heap_free += GET_SIZE(q);
    FWADDR(q) = (int)addr;
    SET_FRWD(q);

    return addr + (p-q);
}

/* すべてのポインタがto_spaceに収まっていることを確認 */
void validate(void) {
    static int num;
    int *scan = to_space;

    printf("validate:%d\n", num++);
    fflush(stdout);
    while (scan < heap_free) {
        int *p;
        if (!IS_ARRY(scan))
            for (p = scan + 1; p < scan + GET_SIZE(scan); p++) {
                if (IS_VALUE((int*)*p)) continue;
                if (IS_NOPTR((int*)*p)) continue;
                if (!((int*)*p >= to_space && (int*)*p < heap_top)) {
                    printf("p=%p,*p=%p --- <%p:%p>\n",
                           p, (int*)*p,
                           to_space, heap_top);
                    fflush(stdout);
                    assert(0);
                }
            }
        scan += GET_SIZE(scan);
    }
}

