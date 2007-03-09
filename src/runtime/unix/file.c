/**
 * @file 
 * @brief ファイル入出力処理(実行時ライブラリ)
 *
 * @author Kenta HATTORI
 * @date   2006/04/26
 * $Id: file.c,v 1.2 2006/08/07 09:07:51 hattori Exp $
 */
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <errno.h>
#include <assert.h>
#include "prcrt.h"
#include "exec.h"
#include "perr.h"
#include "file.h"
#include "io.h"

/*
 * ファイルを読み込み専用にオープンする
 */
int prc_FileOpenR(int ich, char *fname) {
    struct stat st;
    int fd;

    if (stat(fname, &st) < 0) {
        return -1;
        // perr(PERR_SYSTEM, "stat", strerror(errno), __FILE__, __LINE__);
    }
    if (!S_ISREG(st.st_mode)) {
        return -1;
    }
    if ((fd = open(fname, O_RDONLY)) < 0) {
        // perr(PERR_SYSTEM, "open", strerror(errno), __FILE__, __LINE__);
	return -1;
    }
    // printf("file opened: %d\n", fd);
    ioent_create((chan_t *)ich, fd, IO_TYPE_IN, BUFSIZ);

    return 0;
}

/*
 * ファイルを新規作成する
 */
int prc_FileCreate(int och, char *fname) {
    int fd;

    if ((fd = creat(fname,S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH)) < 0) {
        perr(PERR_SYSTEM, "open", strerror(errno), __FILE__, __LINE__);
	return -1;
    }

    ioent_create((chan_t *)och, fd, IO_TYPE_OUT, BUFSIZ);

    return 0;
}
