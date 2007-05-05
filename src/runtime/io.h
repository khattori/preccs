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
#include "type.h"
#include "queue.h"

#ifdef WIN32
typedef HANDLE iohandle_t;
typedef OVERLAPPED ioctlblk_t;
#else
typedef int iohandle_t;
typedef struct aiocb ioctlblk_t;
#endif

typedef enum {
    IO_TYPE_IN,
    IO_TYPE_OUT,
    IO_TYPE_TIMER,
    IO_TYPE_ACCEPT,
    IO_TYPE_CONNECT
} iotype_t;

struct ioent_ {
    TAILQ_ENTRY(ioent_) link;  /* リンク */
    TAILQ_ENTRY(ioent_) mlink; /* 多重IO用リンク */
    iotype_t type;             /* IO種別 */
    chan_t *chan;
    iohandle_t handle;
    ioctlblk_t ctlblk;
    size_t offset;
    size_t bufsz;
    char buf[1];
};

typedef TAILQ_HEAD(ioq_, ioent_) ioq_t;

void io_init(void);
int  io_exec(void);
void io_chout(ioent_t *io, event_t *evt);
void io_chin(ioent_t *io, event_t *evt);

void ioent_create(chan_t *ch, iohandle_t handle, iotype_t type, size_t size);
void ioent_delete(ioent_t *ioent);
void ioent_delete2(iohandle_t handle);

#endif /* __INC_IO_H__ */
