/**
 * @file 
 * @brief エラー処理機構(実行時ライブラリ)
 *
 * @author Kenta HATTORI
 * @date   2005/07/05
 * $id$
 */
#ifndef __INC_PERR_H__
#define __INC_PERR_H__

typedef enum {
#define DEF_PERR(err, msg, cls) err,
#include "perr.def"
#undef DEF_PERR
} perr_t;

void perr(perr_t err, ...);

#endif /* __INC_PERR_H__ */
