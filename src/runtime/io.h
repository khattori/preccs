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

#ifdef WIN32
#include <windows.h>
#else
#include <aio.h>
#endif
#include "event.h"
#include "type.h"
#include "queue.h"

#ifdef WIN32
typedef HANDLE iohandle_t;
typedef OVERLAPPED ioctlblk_t;
#else
typedef int iohandle_t;
typedef struct aiocb ioctlblk_t;
#endif

typedef void (*iof_t)(ioent_t *,event_t *,int exec);
typedef enum { IOT_INPUT, IOT_OUTPUT, IOT_EVENT } iotype_t;

struct ioent_ {
    TAILQ_ENTRY(ioent_) link;  /* リンク */
    TAILQ_ENTRY(ioent_) mlink; /* 多重IO用リンク */
    iotype_t iotype;           /* IO種別 */
    iof_t iof;                 /* IO処理関数へのポインタ */
    chan_t *chan;              /* チャネルへのリンク */
    iohandle_t handle;
    ioctlblk_t ctlblk;
    size_t offset;
    size_t bufsz;
    char buf[1];
};

typedef TAILQ_HEAD(ioq_, ioent_) ioq_t;

extern ioq_t __prc__ioq;
extern ioq_t __prc__mioq;
extern int aio_count;

void io_init(void);
int  io_exec(void);

void io_read_complete(ioent_t *io, int len);
void io_write_complete(ioent_t *io, int len);

void ioent_create(chan_t *ch, iohandle_t handle, iotype_t iotype, iof_t iof, size_t size);
void ioent_delete(ioent_t *ioent);
void ioent_delete2(iohandle_t handle);

#endif /* __INC_IO_H__ */
