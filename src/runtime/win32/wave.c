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
#include "proc.h"
#include "exec.h"
#include "perr.h"
#include "strerror.h"
#include "wave.h"

#define RBUF_NUM 5
#define RBUF_SIZ BUFSIZ

static int woq_len; /* 出力キューの長さ */
static int woq_hd;
static int woq_tl;

static int wiq_len; /* 入力キューの長さ */
static int wiq_hd;
static int wiq_tl;

static WAVEHDR wo_whdr[RBUF_NUM];
static WAVEHDR wi_whdr[RBUF_NUM];

static void wv_input(ioent_t *io, event_t *evt, int exec);
static void wv_output(ioent_t *io, event_t *evt, int exec);
static void wout_exec(ioent_t *io, event_t *evt);

static void CALLBACK waveInProc(HWAVEOUT hwi, UINT uMsg, DWORD dwInstance, DWORD dwParam1, DWORD dwParam2) {
    ioent_t *io = (ioent_t *)dwInstance;
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
        if (!SetEvent(io->ctlblk.hEvent)) {
            perr(PERR_SYSTEM, "SetEvent", StrError(GetLastError()), __FILE__, __LINE__);
            return;
        }
        break;
    case WIM_OPEN:
        break;
    }
}
/**
 * サウンド入力用デバイスをオープン
 */
int prc_WaveInOpen(int ch, int srate) {
    WAVEFORMATEX wfe;
    MMRESULT ret;
    ioent_t *io;
    int i;

    wfe.wFormatTag      = WAVE_FORMAT_PCM;
    wfe.nChannels       = 1;
    wfe.nSamplesPerSec  = srate;
    wfe.nAvgBytesPerSec = srate;
    wfe.wBitsPerSample  = 16;
    wfe.nBlockAlign     = wfe.nChannels * wfe.wBitsPerSample / 8;

    io = ioent_create((chan_t *)ch, NULL, IOT_INPUT, wv_input, RBUF_SIZ*RBUF_NUM);

    ret = waveInOpen((LPHWAVEIN)&io->handle, WAVE_MAPPER, &wfe, (DWORD)waveInProc, (DWORD)io, CALLBACK_FUNCTION);
    if (ret != MMSYSERR_NOERROR) {
        perr(PERR_SYSTEM2, "waveInOpen", ret, __FILE__, __LINE__);
        return -1;
    }

    for (i = 0; i < RBUF_NUM; i++) {
        wi_whdr[i].lpData          = io->buf + RBUF_SIZ*i;
        wi_whdr[i].dwBufferLength  = RBUF_SIZ;
        wi_whdr[i].dwBytesRecorded = 0;
        wi_whdr[i].dwFlags         = WHDR_BEGINLOOP | WHDR_ENDLOOP;
        wi_whdr[i].dwLoops         = 1;
        wi_whdr[i].lpNext          = NULL;
        wi_whdr[i].dwUser          = i;
        wi_whdr[i].reserved        = 0;

        ret = waveInPrepareHeader(io->handle, &wi_whdr[i], sizeof(WAVEHDR));
        if (ret != MMSYSERR_NOERROR) {
            perr(PERR_SYSTEM2, "waveInPrepareHeader", ret, __FILE__, __LINE__);
            return -1;
        }
        ret = waveInAddBuffer(io->handle, &wi_whdr[i], sizeof(WAVEHDR));
        if (ret != MMSYSERR_NOERROR) {
            perr(PERR_SYSTEM2, "waveInAddBuffer", ret, __FILE__, __LINE__);
            return -1;
        }
    }
    ret = waveInStart(io->handle);
    if (ret != MMSYSERR_NOERROR) {
        perr(PERR_SYSTEM2, "waveInStart", ret, __FILE__, __LINE__);
        return -1;
    }

    return 0;
}

static void CALLBACK waveOutProc(HWAVEOUT hwo, UINT uMsg, DWORD dwInstance, DWORD dwParam1, DWORD dwParam2) {
    ioent_t *io = (ioent_t *)dwInstance;
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
        if (!SetEvent(io->ctlblk.hEvent)) {
            perr(PERR_SYSTEM, "SetEvent", StrError(GetLastError()), __FILE__, __LINE__);
            return;
        }
        break;
    case WOM_OPEN:
        break;
    }
}
/**
 * サウンド出力用デバイスをオープン
 */
int prc_WaveOutOpen(int ch, int srate) {
    WAVEFORMATEX wfe;
    MMRESULT ret;
    ioent_t *io;
    int i;

    wfe.wFormatTag      = WAVE_FORMAT_PCM;
    wfe.nChannels       = 1;
    wfe.nSamplesPerSec  = srate;
    wfe.nAvgBytesPerSec = srate;
    wfe.wBitsPerSample  = 16;
    wfe.nBlockAlign     = wfe.nChannels * wfe.wBitsPerSample / 8;

    io = ioent_create((chan_t *)ch, NULL, IOT_OUTPUT, wv_output, RBUF_SIZ*RBUF_NUM);

    ret = waveOutOpen((LPHWAVEOUT)&io->handle, WAVE_MAPPER, &wfe, (DWORD)waveOutProc, (DWORD)io, CALLBACK_FUNCTION);
    if (ret != MMSYSERR_NOERROR) {
        perr(PERR_SYSTEM2, "waveOutOpen", ret, __FILE__, __LINE__);
        return -1;
    }
    for (i = 0; i < RBUF_NUM; i++) {
        wo_whdr[i].lpData = io->buf + RBUF_SIZ*i;
        wo_whdr[i].dwBufferLength = RBUF_SIZ;
        wo_whdr[i].dwBytesRecorded = 0;
        wo_whdr[i].dwFlags = WHDR_BEGINLOOP | WHDR_ENDLOOP;
        wo_whdr[i].dwLoops = 1;
        wo_whdr[i].lpNext = NULL;
        wo_whdr[i].dwUser = i;
        wo_whdr[i].reserved = 0;

        ret = waveOutPrepareHeader(io->handle, &wo_whdr[i], sizeof(WAVEHDR));
        if (ret != MMSYSERR_NOERROR) {
            perr(PERR_SYSTEM2, "waveOutPrepareHeader", ret, __FILE__, __LINE__);
            return -1;
        }
    }

    return 0;
}

/* サウンド入力を閉じる */
static void win_close(ioent_t *io) {
    MMRESULT ret;
    int i;
    // printf("wave in close\n");
    // fflush(stdout);
    
    ret = waveInReset(io->handle);
    if (ret != MMSYSERR_NOERROR) {
        perr(PERR_SYSTEM2, "waveInReset", ret, __FILE__, __LINE__);
        return;
    }
    for (i = 0; i < RBUF_NUM; i++) {
        ret = waveInUnprepareHeader(io->handle, &wi_whdr[i], sizeof(WAVEHDR));
        if (ret != MMSYSERR_NOERROR) {
            perr(PERR_SYSTEM2, "waveOutUnprepareHeader", ret, __FILE__, __LINE__);
            return;
        }
    }
    ret = waveInClose(io->handle);
    if (ret != MMSYSERR_NOERROR) {
        perr(PERR_SYSTEM2, "waveInClose", ret, __FILE__, __LINE__);
        return;
    }
    wiq_len = 0;
    wiq_tl  = 0;
    wiq_hd  = 0;
}

static void wv_read_complete(ioent_t *io, int num) {
    proc_t *prc;
    event_t *evt;

    __prc__regs[0] = __string__(RBUF_SIZ, (void *)io->buf+RBUF_SIZ*num);
    prc = proc();
    evt = chin_next(io->chan);
    prc->clos = evt->clos;
    prc->val  = __prc__regs[0];
    TAILQ_INSERT_TAIL(__prc__rdyq, prc, link);
    TAILQ_REMOVE(&io->chan->inq, evt, link);
}

static void wv_input(ioent_t *io, event_t *evt, int exec) {
    MMRESULT ret;

    /* サウンド入力処理 */
    if (chin_next(io->chan) == evt) {
        // 未処理録音データがある場合
        if (evt->trans == 0 && wiq_len > 0) {
            int i = wiq_hd++;
            wiq_hd %= RBUF_NUM;
            wiq_len--;

            wv_read_complete(io, i);
            ret = waveInAddBuffer(io->handle, &wi_whdr[i], sizeof(WAVEHDR));
            if (ret != MMSYSERR_NOERROR) {
                perr(PERR_SYSTEM2, "waveInAddBuffer", ret, __FILE__, __LINE__);
                return;
            }
            /* IO待ちプロセスが無ければ終了 */
            if ((evt = chin_next(io->chan)) == NULL) {
                return;
            }
            io->iof(io, evt, 0);
        } else {
            TAILQ_INSERT_TAIL(&__prc__mioq, io, mlink);
        }
    } else if (chout_next(io->chan) == evt) {
        win_close(io);
        io_write_complete(io, 0);
        ioent_delete(io);
    }
}

/* サウンド出力を閉じる */
static void wout_close(ioent_t *io) {
    MMRESULT ret;
    int i;
    // printf("wave out close\n");
    // fflush(stdout);

    ret = waveOutReset(io->handle);
    if (ret  != MMSYSERR_NOERROR) {
        perr(PERR_SYSTEM2, "waveOutReset", ret, __FILE__, __LINE__);
        return;
    }
    for (i = 0; i < RBUF_NUM; i++) {
        ret = waveOutUnprepareHeader(io->handle, &wo_whdr[i], sizeof(WAVEHDR));
        if (ret != MMSYSERR_NOERROR) {
            perr(PERR_SYSTEM2, "waveOutUnprepareHeader", ret, __FILE__, __LINE__);
            return;
        }
    }
    ret = waveOutClose(io->handle);
    if (ret != MMSYSERR_NOERROR) {
        perr(PERR_SYSTEM2, "waveOutClose", ret, __FILE__, __LINE__);
        return;
    }
    woq_len = 0;
    woq_hd  = 0;
    woq_tl  = 0;
}

static void wv_output(ioent_t *io, event_t *evt, int exec) {
    if (evt->trans == 0) {
        int len = STRLEN(evt->val);
        if (len == 0) {
            wout_close(io);
            io_write_complete(io, 0);
            ioent_delete(io);
            return;
        }
        io->offset = 0;
        wout_exec(io, evt);
        return;
    }
    TAILQ_INSERT_TAIL(&__prc__mioq, io, mlink);
}

/* 一つのオーディオ出力データブロックを処理する */
static void wout_exec(ioent_t *io, event_t *evt) {
    MMRESULT ret;
    int len;

    if (woq_len < RBUF_NUM) {
        int i = woq_tl++;
        woq_tl %= RBUF_NUM;
        woq_len++;
        len = STRLEN(evt->val)-io->offset;
        if (len > RBUF_SIZ) {
            len = io->bufsz;
        }
        /* waveOutWrite処理 */
        memcpy(wo_whdr[i].lpData, STRPTR(evt->val)+io->offset, len);
        wo_whdr[i].dwBufferLength = len;
        ret = waveOutWrite(io->handle, &wo_whdr[i], sizeof(WAVEHDR));
        if (ret != MMSYSERR_NOERROR) {
            perr(PERR_SYSTEM, "waveOutWrite", ret, __FILE__, __LINE__);
            return;
        }
        io->offset += len;
        if (io->offset < STRLEN(evt->val)) {
            wout_exec(io, evt);
            return;
        }
        io_write_complete(io, len);
        /* IO待ちプロセスが無ければ終了 */
        if ((evt = chout_next(io->chan)) != NULL) {
            io->iof(io, evt, 0);
        }
    } else {
        TAILQ_INSERT_TAIL(&__prc__mioq, io, mlink);
    }
}

