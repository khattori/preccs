/**
 * @file 
 * @brief サウンド処理(実行時ライブラリ)
 *
 * @author Kenta HATTORI
 * @date   2006/08/06
 * $Id: wave.h,v 1.1 2006/08/07 09:07:51 hattori Exp $
 */
#ifndef __INC_WAVE_H__
#define __INC_WAVE_H__

int wave_io(HANDLE handles[], ioent_t io_table[], int *io_count);
int prc_WaveOutOpen(int ch, int srate);
int prc_WaveInOpen(int ch, int srate);

#endif /* __INC_WAVE_H__ */
