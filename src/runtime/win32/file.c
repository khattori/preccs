/**
 * @file 
 * @brief ファイル入出力処理(実行時ライブラリ)
 *
 * @author Kenta HATTORI
 * @date   2006/04/26
 * $Id: file.c,v 1.2 2006/08/07 09:07:51 hattori Exp $
 */
#include <windows.h>
#include <stdio.h>
#include <stddef.h>
#include <assert.h>
#include "prcrt.h"
#include "exec.h"
#include "perr.h"
#include "strerror.h"
#include "file.h"

static void fi_input(ioent_t *io, event_t *evt, int exec);
static void fi_output(ioent_t *io, event_t *evt, int exec);
static void write_exec(ioent_t *io, event_t *evt);

/*
 * ファイルを読み込み専用にオープンする
 */
int prc_FileOpenR(int ich, char *fname) {
    HANDLE h;

    h = CreateFile(fname,
                   GENERIC_READ,
                   FILE_SHARE_READ,
                   NULL,
                   OPEN_EXISTING,
                   FILE_FLAG_OVERLAPPED,
                   NULL);
    if (h == INVALID_HANDLE_VALUE) {
        // fprintf(stderr, "CreateFile():%s\n", StrError(GetLastError()));
        return -1;
    }
    ioent_create((chan_t *)ich, h, IOT_INPUT, fi_input, BUFSIZ);

    return 0;
}

/*
 * ファイルを新規作成する
 */
int prc_FileCreate(int och, char *fname) {
    HANDLE h;

    h = CreateFile(fname,
                   GENERIC_WRITE,
                   FILE_SHARE_READ,
                   NULL,
                   CREATE_ALWAYS,
                   FILE_FLAG_OVERLAPPED,
                   NULL);
    if (h == INVALID_HANDLE_VALUE) {
        perr(PERR_SYSTEM, "CreateFile", StrError(GetLastError()), __FILE__, __LINE__);
        return -1;
    }

    ioent_create((chan_t *)och, h, IOT_OUTPUT, fi_output, BUFSIZ);

    return 0;
}

static void close_file(ioent_t *io) {
//    printf("close_file: enter\n");
//    fflush(stdout);

    if (!CloseHandle(io->handle)) {
        perr(PERR_SYSTEM, "CloseHandle", StrError(GetLastError()), __FILE__, __LINE__);
        return ;
    }

    ioent_delete(io);
}

static void CALLBACK read_completion_handler(DWORD err, DWORD len, LPOVERLAPPED ovl) {
    event_t *evt;
    ioent_t *io = (ioent_t*)(((char*)ovl)-offsetof(ioent_t,ctlblk));

//    printf("read completed: err=%ld, len=%ld\n", err, len);
//    fflush(stdout);

    aio_count--;
    io_read_complete(io, len);
    /* IO待ちプロセスが無ければ終了 */
    if ((evt = chin_next(io->chan)) == NULL) {
        return;
    }
    io->iof(io, evt, 0);
}
/**
 * IOチャネル入力時の処理 
 */
static void fi_input(ioent_t *io, event_t *evt, int exec) {
    DWORD error;

//    printf("fi_input: enter(exec=%d, trans=%d)\n", exec, evt->trans);
//    fflush(stdout);

    if (evt->trans == 0) {
        aio_count++;
        /* 新規に読み込みIO発行 */
        if (!ReadFileEx(io->handle, io->buf, io->bufsz, &io->ctlblk, read_completion_handler)) {
            error = GetLastError();
            switch (error) {
            case ERROR_IO_PENDING:
                break;
            case ERROR_HANDLE_EOF:
                aio_count--;
                io_read_complete(io, 0);
                close_file(io);
                break;
            default:
                perr(PERR_SYSTEM, "ReadFile", StrError(error), __FILE__, __LINE__);
                return;
            }
        }
        return;
    }

    if (!SetEvent(io->ctlblk.hEvent)) {
        error = GetLastError();
        perr(PERR_SYSTEM, "SetEvent", StrError(error), __FILE__, __LINE__);
        return;
    }
    TAILQ_INSERT_TAIL(&__prc__mioq, io, mlink);
}

static void CALLBACK write_completion_handler(DWORD err, DWORD len, LPOVERLAPPED ovl) {
    event_t *evt;
    ioent_t *io = (ioent_t*)(((char*)ovl)-offsetof(ioent_t,ctlblk));

//    printf("write completed: err=%ld, len=%ld\n", err, len);
//    fflush(stdout);

    aio_count--;
    io->offset += len;
    evt = (event_t*)chout_next(io->chan);
    if (io->offset < STRLEN(evt->val)) {
        write_exec(io, evt);
        return;
    }
    io_write_complete(io, len);
    /* IO待ちプロセスが無ければ終了 */
    if ((evt = chout_next(io->chan)) != NULL) {
        io->iof(io, evt, 0);
    }
}
static void write_exec(ioent_t *io, event_t *evt) {
    DWORD error;
    int len;

    aio_count++;
    len = STRLEN(evt->val) - io->offset;
    if (len > io->bufsz) {
        len = io->bufsz;
    }
    /* 新規に書き込みIO発行 */
    memcpy(io->buf, STRPTR(evt->val)+io->offset, len);
    if (!WriteFileEx(io->handle, io->buf, len, &io->ctlblk, write_completion_handler)) {
        error = GetLastError();
        switch (error) {
        case ERROR_IO_PENDING:
            break;
        default:
            perr(PERR_SYSTEM, "WriteFile", error, __FILE__, __LINE__);
            return;
        }
    }
}

/**
 * IOチャネル出力時の処理 
 */
static void fi_output(ioent_t *io, event_t *evt, int exec) {
    DWORD error;

//    printf("fi_output: enter(exec=%d,trans=%d)\n", exec, evt->trans);
//    fflush(stdout);

    if (evt->trans == 0) {
        int len = STRLEN(evt->val);
        if (len == 0) {
            io_write_complete(io, 0);
            close_file(io);
            return;
        }
        io->offset = 0;
        write_exec(io, evt);
        return;
    }

    if (!SetEvent(io->ctlblk.hEvent)) {
        error = GetLastError();
        perr(PERR_SYSTEM, "SetEvent", error, __FILE__, __LINE__);
        return;
    }
    TAILQ_INSERT_TAIL(&__prc__mioq, io, mlink);
}
