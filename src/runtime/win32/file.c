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
#include "strerror.h"
#include "file.h"

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
    ioent_create((chan_t *)ich, h, IO_TYPE_IN, BUFSIZ);

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

    ioent_create((chan_t *)och, h, IO_TYPE_OUT, BUFSIZ);

    return 0;
}
