/**
 * @file 
 * @brief エラー処理機構(実行時ライブラリ)
 *
 * @author Kenta Hattori
 * @date   2005/07/05
 * $Id: perr.c,v 1.1 2006/05/25 06:53:51 hattori Exp $
 */
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "perr.h"

/*
 * エラークラスの定義
 */
#define PERRCLS_FATAL 0
#define PERRCLS_ERROR 1
#define PERRCLS_WARN  2

/*
 * エラー一覧の定義
 */
struct {
    int   cls;
    char *msg;
} errtbl[] = {
#define DEF_PERR(err, msg, cls) { cls, msg },
#include "perr.def"
#undef DEF_PERR
};

/**
 *  エラーメッセージを表示する
 *
 *  @param err [in] エラー種別
 *  @param ... [in] エラーメッセージのパラメータ
 *  @return なし
 *
 *  @note FATALの場合はexit(1)する。
 *
 */
void perr(perr_t err, ...) {
    va_list ap;
    int cls   = errtbl[err].cls;
    char *msg = errtbl[err].msg;

    va_start(ap, err);
    vfprintf(stderr, msg, ap);
    fprintf(stderr, "\n");
    fflush(stderr);
    va_end(ap);

    /* FATAL時は終了 */
    if (cls == PERRCLS_FATAL) {
	exit(1);
    }
}

