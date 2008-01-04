/**
 * @file 
 * @brief プロセス実行エンジン(実行時ライブラリ)
 *
 * @author Kenta HATTORI
 * @date   2006/04/18
 * $Id: exec.h,v 1.2 2006/07/27 00:06:56 hattori Exp $
 */
#ifndef __INC_EXEC_H__
#define __INC_EXEC_H__

int __send__(void);
int __null_send__(void);
int __cond_send__(void);
int __recv__(void);
int __null_recv__(void);

#endif /* __INC_EXEC_H__ */
