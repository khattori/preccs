/**
 * @file 
 * @brief サウンド処理(実行時ライブラリ)
 *
 * @author Kenta HATTORI
 * @date   2006/08/06
 * $Id: wave.c,v 1.1 2006/08/07 09:07:51 hattori Exp $
 */
#include <windows.h>
#include <stdio.h>
#include "prcrt.h"
#include "exec.h"
#include "perr.h"
#include "wave.h"

#define RBUF_NUM 5
#define RBUF_SIZ BUFSIZ

static HWAVEOUT hWaveOut = NULL;
static HWAVEIN  hWaveIn  = NULL;
static HANDLE hEvtWout = NULL;
static HANDLE hEvtWin  = NULL;

int wave_och; /* GCの対象 */
int wave_ich; /* GCの対象 */
static int woq_len; /* 出力キューの長さ */
static int woq_hd;
static int woq_tl;

static int wiq_len; /* 入力キューの長さ */
static int wiq_hd;
static int wiq_tl;

static WAVEHDR wo_whdr[RBUF_NUM];
static WAVEHDR wi_whdr[RBUF_NUM];
static char rec_bufs[RBUF_NUM][RBUF_SIZ];

/* IO処理ループから呼び出される */
int wave_io(HANDLE handles[], ioent_t io_table[], int *io_count) {
    event_t *evt;
    int len;
    MMRESULT ret;

    /* waveOutWrite処理 */
    if (wave_och) {
        if ((evt = chout_next((chan_t *)wave_och)) != NULL
            && woq_len < RBUF_NUM) {
            int i = woq_tl++;
            char *buf;
            
            if ((len = STRLEN(evt->val)) == 0)
                return chan_recv(wave_och);

            if (wo_whdr[i].lpData) {
                ret = waveOutUnprepareHeader(hWaveOut, &wo_whdr[i], sizeof(WAVEHDR));
                if (ret != MMSYSERR_NOERROR) {
                    perr(PERR_SYSTEM, "waveOutUnprepareHeader", ret, __FILE__, __LINE__);
                }
                free(wo_whdr[i].lpData);
            }

            if ((buf = (char *)malloc(len)) == NULL) {
                perr(PERR_OUTOFMEM, __FILE__, __LINE__);
            }
            memcpy(buf, STRPTR(evt->val), len);

            wo_whdr[i].lpData = buf;
            wo_whdr[i].dwBufferLength = len;
            wo_whdr[i].dwBytesRecorded = 0;
            wo_whdr[i].dwFlags = WHDR_BEGINLOOP | WHDR_ENDLOOP;
            wo_whdr[i].dwLoops = 1;
            wo_whdr[i].lpNext = NULL;
            wo_whdr[i].dwUser = i;
            wo_whdr[i].reserved = 0;

            woq_tl %= RBUF_NUM;
            woq_len++;

            ret = waveOutPrepareHeader(hWaveOut, &wo_whdr[i], sizeof(WAVEHDR));
            if (ret != MMSYSERR_NOERROR) {
                perr(PERR_SYSTEM, "waveOutPrepareHeader", ret, __FILE__, __LINE__);
            }
            ret = waveOutWrite(hWaveOut, &wo_whdr[i], sizeof(WAVEHDR));
            if (ret != MMSYSERR_NOERROR) {
                perr(PERR_SYSTEM, "waveOutWrite", ret, __FILE__, __LINE__);
            }

            return chan_recv(wave_och);
        } else {
            ioent_t *io = &io_table[*io_count];

            ResetEvent(hEvtWout);
            handles[*io_count] = hEvtWout;
            io->type = IOT_WAVE;
            (*io_count)++;
        }
        /* waveOutClose処理 */
        if ((evt = chin_next((chan_t *)wave_och)) != NULL) {
            int i;
            printf("wave out close\n");
            fflush(stdout);

            if ((ret = waveOutReset(hWaveOut)) != MMSYSERR_NOERROR) {
                perr(PERR_SYSTEM, "waveOutReset", ret, __FILE__, __LINE__);
            }

            for (i = 0; i < RBUF_NUM; i++) {
                if (wo_whdr[i].lpData == NULL) continue;

                free(wo_whdr[i].lpData);
                ret = waveOutUnprepareHeader(hWaveOut, &wo_whdr[i], sizeof(WAVEHDR));
                if (ret != MMSYSERR_NOERROR) {
                    perr(PERR_SYSTEM, "waveOutUnprepareHeader", ret, __FILE__, __LINE__);
                }
            }

            if ((ret = waveOutClose(hWaveOut)) != MMSYSERR_NOERROR) {
                perr(PERR_SYSTEM, "waveOutClose", ret, __FILE__, __LINE__);
            }
            hWaveOut = NULL;
            __prc__regs[0] = __string__(0, "");
            __prc__regs[1] = wave_och;
            wave_och = 0;
            woq_len = 0;
            woq_hd  = 0;
            woq_tl  = 0;
            return chan_send(__prc__regs[1], __prc__regs[0]);
        }
    }
    /* サウンド入力処理 */
    if (wave_ich) {
        if (chin_next((chan_t *)wave_ich) != NULL) {
            if (wiq_len > 0) { // 未処理録音データがある場合
                int i = wiq_hd++;

                wiq_hd %= RBUF_NUM;
                wiq_len--;
                __prc__regs[0] = __string__(wi_whdr[i].dwBytesRecorded, wi_whdr[i].lpData);
                ret = waveInAddBuffer(hWaveIn, &wi_whdr[i], sizeof(WAVEHDR));
                if (ret != MMSYSERR_NOERROR) {
                    perr(PERR_SYSTEM, "waveInAddBuffer", ret, __FILE__, __LINE__);
                }

                return chan_send(wave_ich, __prc__regs[0]);
            } else {           // 未処理録音データがない場合
                ioent_t *io = &io_table[*io_count];
                ResetEvent(hEvtWin);
                handles[*io_count] = hEvtWin;
                io->type = IOT_WAVE;
                (*io_count)++;
            }
        }
        /* waveInClose処理 */
        if (chout_next((chan_t *)wave_ich) != NULL) {
            int i;
            printf("wave in close\n");
            fflush(stdout);
            if ((ret = waveInReset(hWaveIn)) != MMSYSERR_NOERROR) {
                perr(PERR_SYSTEM, "waveInReset", ret, __FILE__, __LINE__);
            }

            for (i = 0; i < RBUF_NUM; i++) {
                ret = waveInUnprepareHeader(hWaveIn, &wi_whdr[i], sizeof(WAVEHDR));
                if (ret != MMSYSERR_NOERROR) {
                    perr(PERR_SYSTEM, "waveOutUnprepareHeader", ret, __FILE__, __LINE__);
                }
            }

            if ((ret = waveInClose(hWaveIn)) != MMSYSERR_NOERROR) {
                perr(PERR_SYSTEM, "waveInClose", ret, __FILE__, __LINE__);
            }
            hWaveIn = NULL;
            __prc__regs[0] = wave_ich;
            wave_ich = 0;
            wiq_len = 0;
            wiq_tl  = 0;
            wiq_hd  = 0;
            return chan_recv(__prc__regs[0]);
        }
    }
        
    return 0;
}

static void CALLBACK waveOutProc(
    HWAVEOUT hwo,     
    UINT uMsg,        
    DWORD dwInstance, 
    DWORD dwParam1,   
    DWORD dwParam2    
    ) {
    LPWAVEHDR lpwh = (LPWAVEHDR)dwParam1;

    switch (uMsg) {
    case WOM_CLOSE:
        break;
    case WOM_DONE:
        assert(woq_hd == lpwh->dwUser);
        assert(woq_len > 0);

        woq_hd++;
        woq_hd %= RBUF_NUM;
        woq_len--;
        SetEvent(hEvtWout);
        break;
    case WOM_OPEN:
        break;
    }
}

/* サウンド出力用デバイスをオープン */
int prc_WaveOutOpen(int ch, int srate) {
    WAVEFORMATEX wfe;
    MMRESULT ret;

    wfe.wFormatTag      = WAVE_FORMAT_PCM;
    wfe.nChannels       = 1;
    wfe.nSamplesPerSec  = srate;
    wfe.nAvgBytesPerSec = srate;
    wfe.wBitsPerSample  = 16;
    wfe.nBlockAlign     = wfe.nChannels * wfe.wBitsPerSample / 8;

    if ((ret = waveOutOpen(&hWaveOut, WAVE_MAPPER, &wfe,
                           (DWORD)waveOutProc, 0, CALLBACK_FUNCTION)) != MMSYSERR_NOERROR) {
        perr(PERR_SYSTEM, "waveOutOpen", ret, __FILE__, __LINE__);
    }
    if (hEvtWout == NULL
        && (hEvtWout = CreateEvent(NULL, TRUE, FALSE, NULL)) == NULL) {
        perr(PERR_SYSTEM, "CreateEvent", GetLastError(), __FILE__, __LINE__);
    }
    wave_och = ch;
    memset(wo_whdr, 0, sizeof wo_whdr);

    return 0;
}

static void CALLBACK waveInProc(
    HWAVEOUT hwi,     
    UINT uMsg,        
    DWORD dwInstance, 
    DWORD dwParam1,   
    DWORD dwParam2    
    ) {
    LPWAVEHDR lpwh = (LPWAVEHDR)dwParam1;

    switch (uMsg) {
    case WIM_CLOSE:
        break;
    case WIM_DATA:
        assert(wiq_tl == lpwh->dwUser);
        assert(wiq_len < 5);

        wiq_tl++;
        wiq_tl %= RBUF_NUM;
        wiq_len++;
        SetEvent(hEvtWin);
        break;
    case WIM_OPEN:
        break;
    }
}

/* サウンド入力用デバイスをオープン */
int prc_WaveInOpen(int ch, int srate) {
    WAVEFORMATEX wfe;
    MMRESULT ret;
    int i;

    wfe.wFormatTag      = WAVE_FORMAT_PCM;
    wfe.nChannels       = 1;
    wfe.nSamplesPerSec  = srate;
    wfe.nAvgBytesPerSec = srate;
    wfe.wBitsPerSample  = 16;
    wfe.nBlockAlign     = wfe.nChannels * wfe.wBitsPerSample / 8;

    if ((ret = waveInOpen(&hWaveIn, WAVE_MAPPER, &wfe,
                           (DWORD)waveInProc, 0, CALLBACK_FUNCTION)) != MMSYSERR_NOERROR) {
        perr(PERR_SYSTEM, "waveInOpen", ret, __FILE__, __LINE__);
    }

    wave_ich = ch;
    for (i = 0; i < RBUF_NUM; i++) {
        wi_whdr[i].lpData          = rec_bufs[i];
        wi_whdr[i].dwBufferLength  = RBUF_SIZ;
        wi_whdr[i].dwBytesRecorded = 0;
        wi_whdr[i].dwFlags         = WHDR_BEGINLOOP | WHDR_ENDLOOP;
        wi_whdr[i].dwLoops         = 1;
        wi_whdr[i].lpNext          = NULL;
        wi_whdr[i].dwUser          = i;
        wi_whdr[i].reserved        = 0;

        ret = waveInPrepareHeader(hWaveIn, &wi_whdr[i], sizeof(WAVEHDR));
        if (ret != MMSYSERR_NOERROR) {
            perr(PERR_SYSTEM, "waveInPrepareHeader", ret, __FILE__, __LINE__);
        }
        ret = waveInAddBuffer(hWaveIn, &wi_whdr[i], sizeof(WAVEHDR));
        if (ret != MMSYSERR_NOERROR) {
            perr(PERR_SYSTEM, "waveInAddBuffer", ret, __FILE__, __LINE__);
        }
    }

    if ((ret = waveInStart(hWaveIn)) != MMSYSERR_NOERROR) {
        perr(PERR_SYSTEM, "waveInStart", ret, __FILE__, __LINE__);
    }

    if (hEvtWin == NULL
        && (hEvtWin = CreateEvent(NULL, TRUE, FALSE, NULL)) == NULL) {
        perr(PERR_SYSTEM, "CreateEvent", GetLastError(), __FILE__, __LINE__);
    }

    return 0;
}
