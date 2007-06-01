/**
 * @file 
 * @brief サウンド処理(実行時ライブラリ)
 *
 * @author Kenta HATTORI
 * @date   2007/05/29
 * $Id: wave.c,v 1.1 2006/08/07 09:07:51 hattori Exp $
 */
#include "perr.h"
#include "wave.h"

int prc_WaveOutOpen(int ch, int srate) {
    perr(PERR_NOTSUP, "prc_WaveOutOpen");
    return 0;
}
int prc_WaveInOpen(int ch, int srate) {
    perr(PERR_NOTSUP, "prc_WaveInOpen");
    return 0;
}
