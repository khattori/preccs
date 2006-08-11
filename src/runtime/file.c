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
#include <assert.h>
#include "prcrt.h"
#include "exec.h"
#include "perr.h"
#include "file.h"

#define MAX_FILENO 16
static int file_free;
int file_used;
int file_next[MAX_FILENO];

int file_ch_array[MAX_FILENO];
static HANDLE file_array[MAX_FILENO];
static OVERLAPPED ovl_array[MAX_FILENO];
static char *buf_array[MAX_FILENO];

/*
 * ファイルハンドルとチャネルの組を登録する
 */
static int file_register(int ch, HANDLE hFile) {
    HANDLE hEvent;
    int h;

    if ((h = file_free) < 0) {
        assert(0);
        return -1;
    }
    if ((hEvent = CreateEvent(NULL, TRUE, FALSE, NULL)) == NULL) {
        perr(PERR_SYSTEM, "CreateEvent", GetLastError(), __FILE__, __LINE__);
    }
    if ((buf_array[h] = (char *)malloc(BUFSIZ)) == NULL) {
        perr(PERR_OUTOFMEM, __FILE__, __LINE__);
    }
    file_ch_array[h] = ch;
    ovl_array[h].Offset = 0;
    ovl_array[h].OffsetHigh = 0;
    ovl_array[h].hEvent = hEvent;
    file_array[h] = hFile;

    file_free = file_next[h];
    file_next[h] = file_used;
    file_used = h;

    return h;
}
/*
 * ファイルをクローズし，登録削除する
 */
static void file_close(int h) {
    CloseHandle(file_array[h]);
    CloseHandle(ovl_array[h].hEvent);
    free(buf_array[h]);
    if (file_used == h) {
        file_used = file_next[h];
    } else {
        int i;
        for (i = file_used; i >= 0; i = file_next[i]) {
            if (h == file_next[i]) {
                file_next[i] = file_next[h];
                break;
            }
        }
    }
    file_next[h] = file_free;
    file_free = h;
}

void file_init(void) {
    int i;

    for (i = 0; i < MAX_FILENO-1; i++) {
        file_next[i] = i+1;
    }
    file_next[i] = -1;
    file_free = 0;
    file_used = -1;
}

int file_io(HANDLE handles[], ioent_t io_table[], int *io_count) {
    int i;

    for (i = file_used; i >= 0; i = file_next[i]) {
        event_t *evt;
        HANDLE hFile = file_array[i];
        LPOVERLAPPED pOvl = &ovl_array[i];
        char *buf = buf_array[i];
        int ch = file_ch_array[i];
        ioent_t *io = &io_table[*io_count];
        DWORD len;
        DWORD ret;

        /* input channel */
        if (chin_next((chan_t *)ch) != NULL) {
            /* 前回の入力処理が完了した場合のチェック */
            switch (ret = WaitForSingleObject(pOvl->hEvent, 0)) {
            case WAIT_OBJECT_0:
                if(!GetOverlappedResult(hFile, pOvl, &len, FALSE)) {
                    perr(PERR_SYSTEM, "GetOverlappedResult", GetLastError(), __FILE__, __LINE__);
                }
                pOvl->Offset += len;
                __prc__regs[0] = __string__(len, buf);
                return chan_send(ch, __prc__regs[0]);
            case WAIT_TIMEOUT:
                break;
            case WAIT_ABANDONED:
            case WAIT_FAILED:
                perr(PERR_SYSTEM, "WaitForSingleObject", ret, __FILE__, __LINE__);
            }
            /* 初回の入力処理の場合 */
            if (ReadFile(hFile, buf, BUFSIZ, &len, pOvl)) {
                pOvl->Offset += len;
                ResetEvent(pOvl->hEvent);
                __prc__regs[0] = __string__(len, buf);
                return chan_send(ch, __prc__regs[0]);
            } else {
                switch (ret = GetLastError()) {
                case ERROR_HANDLE_EOF:
                    file_close(i);
                    __prc__regs[0] = __string__(0, "");
                    return chan_send(ch, __prc__regs[0]);
                case ERROR_IO_PENDING:
                    handles[*io_count] = pOvl->hEvent;
                    io->type   = IOT_FILE;
                    io->handle = i;
                    (*io_count)++;
                    break;
                default:
                    perr(PERR_SYSTEM, "ReadFile", ret, __FILE__, __LINE__);
                }
            }
        }
        /* output channel */
        else if ((evt = chout_next((chan_t *)ch)) != NULL) {
            /* 前回の出力処理が完了した場合のチェック */
            switch (ret = WaitForSingleObject(pOvl->hEvent, 0)) {
            case WAIT_OBJECT_0:
                if(!GetOverlappedResult(hFile, pOvl, &len, FALSE)) {
                    perr(PERR_SYSTEM, "GetOverlappedResult", GetLastError(), __FILE__, __LINE__);
                }
                pOvl->Offset += len;
                return chan_recv(ch);
            case WAIT_TIMEOUT:
                break;
            case WAIT_ABANDONED:
            case WAIT_FAILED:
                perr(PERR_SYSTEM, "WaitForSingleObject", ret, __FILE__, __LINE__);
            }
            /* 初回の出力処理の場合 */
            len = STRLEN(evt->val);
            if (len == 0) {
                file_close(i);
                return chan_recv(ch);
            }
            if (len > BUFSIZ) {
                perr(PERR_BUFOVRFLW, __FILE__, __LINE__);
            }
            memcpy(buf, STRPTR(evt->val), len);
            if (WriteFile(hFile, buf, len, NULL, pOvl)) {
                pOvl->Offset += len;
                ResetEvent(pOvl->hEvent);
                return chan_recv(ch);
            } else {
                switch (ret = GetLastError()) {
                case ERROR_IO_PENDING:
                    handles[*io_count] = pOvl->hEvent;
                    io->type   = IOT_FILE;
                    io->handle = i;
                    (*io_count)++;
                    break;
                default:
                    perr(PERR_SYSTEM, "WriteFile", ret, __FILE__, __LINE__);
                }
            }
        }
    }

    return  0;
}

int file_clos(int h) {
    event_t *evt;
    HANDLE hFile = file_array[h];
    LPOVERLAPPED pOvl = &ovl_array[h];
    int ch = file_ch_array[h];
    char *buf = buf_array[h];
    DWORD len;

    if (chin_next((chan_t *)ch) != NULL) {
        if(!GetOverlappedResult(hFile, pOvl, &len, FALSE)) {
            perr(PERR_SYSTEM, "GetOverlappedResult", GetLastError(), __FILE__, __LINE__);
        }
        pOvl->Offset += len;
        ResetEvent(pOvl->hEvent);
        __prc__regs[0] = __string__(len, buf);
        return chan_send(ch, __prc__regs[0]);
    } else if ((evt = chout_next((chan_t *)ch)) != NULL) {
        if(!GetOverlappedResult(hFile, pOvl, &len, FALSE)) {
            perr(PERR_SYSTEM, "GetOverlappedResult", GetLastError(), __FILE__, __LINE__);
        }
        pOvl->Offset += len;
        ResetEvent(pOvl->hEvent);
        return chan_recv(ch);
    }
    return (int)__disp__;
}

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
        fprintf(stderr, "CreateFile():%ld\n", GetLastError());
        return -1;
    }
    return file_register(ich, h);
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
        fprintf(stderr, "CreateFile():%ld\n", GetLastError());
        return -1;
    }
    return file_register(och, h);
}
