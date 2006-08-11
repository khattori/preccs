/**
 * @file 
 * @brief I/O����(���s�����C�u����)
 *
 * @author Kenta HATTORI
 * @date   2006/04/26
 * $Id: io.c,v 1.8 2006/08/07 09:07:51 hattori Exp $
 */
#include <stdio.h>
#include <windows.h>
#include "prcrt.h"
#include "timer.h"
#include "perr.h"
#include "sock.h"
#include "wave.h"
#include "exec.h"
#include "io.h"

static HANDLE hStdOut;
static HANDLE hStdIn;
static HANDLE hEvtInbEmpty;
static HANDLE hEvtInbFull;
static DWORD WINAPI StdinThread(LPVOID param);
static DWORD dwStdinThreadID;
static HANDLE hStdinThread;
static CHAR achInbuf[BUFSIZ];
static DWORD dwInbLen;


static int func_print(void) {
    DWORD len;

    len = STRLEN(__prc__regs[1]);
    if (!WriteFile(hStdOut, (LPCVOID)STRPTR(__prc__regs[1]), len, &len, NULL)) {
        perr(PERR_SYSTEM, "WriteHandle", GetLastError(), __FILE__, __LINE__);
    }

    return (int)__disp__;
}
static int clos_print[1] = { (int)func_print };
static int func_stdout(void) {
    __prc__regs[0] = __prc__recv;
    __prc__regs[1] = __prc__disp;
    __prc__regs[2] = __prc__stdout;
    __prc__regs[3] = (int)clos_print;
    __prc__regs[4] = __record__(1,0);
    return (int)__recv__;
}

static int func_stdin(void) {
    __prc__regs[0] = __prc__send;
    __prc__regs[1] = __prc__disp;
    __prc__regs[2] = __prc__stdin;
    __prc__regs[3] = __string__(dwInbLen, achInbuf);
    __prc__regs[4] = __prc__disp;
    __prc__regs[5] = __record__(1,0);
    /* �o�b�t�@���Ԃ�ʒm */
    if (!SetEvent(hEvtInbEmpty)) {
        perr(PERR_SYSTEM, "SetEvent", GetLastError(), __FILE__, __LINE__);
    }

    return (int)__send__;
}
static int clos_stdin[1] = { (int)func_stdin };

/**
 * I/O�����̏�����
 */
void io_init(void) {
    /* �n���h���̏����� */
    if ((hStdOut = GetStdHandle(STD_OUTPUT_HANDLE)) == INVALID_HANDLE_VALUE) {
        perr(PERR_SYSTEM, "GetStdHandle", GetLastError(), __FILE__, __LINE__);
    }
    if ((hStdIn = GetStdHandle(STD_INPUT_HANDLE)) == INVALID_HANDLE_VALUE) {
        perr(PERR_SYSTEM, "GetStdHandle", GetLastError(), __FILE__, __LINE__);
    }
    if ((hEvtInbEmpty = CreateEvent(NULL, FALSE, TRUE,  NULL)) == NULL) {
        perr(PERR_SYSTEM, "CreateEvent", GetLastError(), __FILE__, __LINE__);
    }
    if ((hEvtInbFull  = CreateEvent(NULL, FALSE, FALSE, NULL)) == NULL) {
        perr(PERR_SYSTEM, "CreateEvent", GetLastError(), __FILE__, __LINE__);
    }
    /* �X���b�h���� */
    hStdinThread = CreateThread(NULL, 0, StdinThread, NULL, 0, &dwStdinThreadID);
    if (hStdinThread == NULL) {
        perr(PERR_SYSTEM, "CreateThread", GetLastError(), __FILE__, __LINE__);
    }
    /* �t�@�C��IO������ */
    file_init();
}
static HANDLE handles[MAXIMUM_WAIT_OBJECTS];
ioent_t io_table[MAXIMUM_WAIT_OBJECTS];
static int io_count;

/**
 * I/O���o�͏��� 
 */
int io_exec(void) {
    event_t *evt;
    ioent_t *io;
    int cont;
    DWORD timo;
    DWORD ret;

    io_count = 0;
    /* �W���o�͑҂� */
    if (chout_next((chan_t *)__prc__stdout) != NULL) {
        return (int)func_stdout;
    }
    /* �W�����͑҂� */
    if (chin_next((chan_t *)__prc__stdin) != NULL) {
        /* �C�x���g��Ԃ𒲂ׂ� */
        ret = WaitForSingleObject(hEvtInbFull,0);
        if (ret == WAIT_OBJECT_0) {
            return (int)func_stdin;
        } else if (ret == WAIT_TIMEOUT) {
            io = &io_table[io_count];
            handles[io_count] = hEvtInbFull; /* �f�[�^������΃C�x���g��� */
            io->type = IOT_STDIN;
            io_count++;
        } else {
            assert(0);
        }
    }
    /* �t�@�C��IO���� */
    if ((cont = file_io(handles, io_table, &io_count)) != 0) {
        return cont;
    }
    /* �\�P�b�gIO���� */
    if ((cont = sock_io(handles, io_table, &io_count)) != 0) {
        return cont;
    }
    /* �T�E���hIO���� */
    if ((cont = wave_io(handles, io_table, &io_count)) != 0) {
        return cont;
    }

    /* �^�C�}�[���� */
    if ((evt = chout_next((chan_t *)__prc__timer)) != NULL) {
        /* �^�C�}�[�L���[�Ɉړ����� */
        /* �C�x���g�͂܂��L�����Z������Ȃ� */
        TAILQ_REMOVE(&((chan_t *)__prc__timer)->outq, evt, link);
        timer_add(evt);
    }
    timo = timer_next();
    if (io_count == 0) {
        if (timo == INFINITE) {
            return (int)__stop__;
        } else {
            Sleep(timo);
            evt = timer_take();
            __prc__regs[0] = evt->clos;
            EV_SET_CANCEL(evt);
        }
    } else {
        ret = WaitForMultipleObjects(io_count, handles, FALSE, timo);
        if (ret < io_count) {
            switch (io_table[ret].type) {
            case IOT_STDIN:
                __prc__regs[0] = (int)clos_stdin;
                break;
            case IOT_SOCK:
                __prc__regs[1] = TOPINT(ret);
                __prc__regs[0] = sock_clos(io_table[ret].handle);
                break;
            case IOT_FILE:
                return file_clos(io_table[ret].handle);
            case IOT_WAVE:
                return (int)__disp__;
            default:
                perr(PERR_INTERNAL, __FILE__, __LINE__);
            }
        } else if (ret == WAIT_TIMEOUT) {
            evt = timer_take();
            __prc__regs[0] = evt->clos;
            EV_SET_CANCEL(evt);
        } else {
            perr(PERR_SYSTEM, "WaitForMultipleObjects", ret, __FILE__, __LINE__);
        }
    }

    return ((int *)__prc__regs[0])[0];
}

/* �W�����͂̊Ď��X���b�h */
static DWORD WINAPI StdinThread(LPVOID param) {
    /* �o�b�t�@����ɂȂ�܂ő҂� */
    while (WaitForSingleObject(hEvtInbEmpty, INFINITE) == WAIT_OBJECT_0) {
        ReadFile(hStdIn, achInbuf, BUFSIZ, &dwInbLen, NULL);
        /* �o�b�t�@���Z�b�g���� */
        if (!SetEvent(hEvtInbFull)) {
            perr(PERR_SYSTEM, "SetEvent", GetLastError(), __FILE__, __LINE__);
        }
    }

    return 0;
}
