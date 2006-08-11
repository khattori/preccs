/**
 * @file 
 * @brief I/O処理(実行時ライブラリ)
 *
 * @author Kenta HATTORI
 * @date   2006/04/26
 * $Id: io.h,v 1.6 2006/08/07 09:07:51 hattori Exp $
 */
#ifndef __INC_IO_H__
#define __INC_IO_H__

#include <windows.h>

void io_init(void);
int io_exec(void);

typedef enum { IOT_STDIN, IOT_SOCK, IOT_FILE, IOT_WAVE } io_t;
typedef struct ioent_ {
    io_t type;      /* IO種別 */
    int  handle;
} ioent_t;

#endif /* __INC_IO_H__ */
