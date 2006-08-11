/**
 * @file 
 * @brief GCモジュール(実行時ライブラリ)
 *
 * @author Kenta HATTORI
 * @date   2006/04/18
 * $Id: gc.h,v 1.2 2006/07/27 00:06:56 hattori Exp $
 */
#ifndef __INC_GC_H__
#define __INC_GC_H__

void __hlimit__(int n);
void gc_init(void);
int *gc_record(int n);
int *gc_array(int n);
int *gc_forward(int *p);

#define GC_ALIGN(l) ((l+3)>>2)

#endif /* __INC_GC_H__ */
