/**
 * @file 
 * @brief ソケット処理(実行時ライブラリ)
 *
 * @author Kenta HATTORI
 * @date   2006/04/26
 * $Id: sock.c,v 1.4 2006/08/07 09:07:51 hattori Exp $
 */
#include <stdio.h>
#include <assert.h>
#include <winsock2.h>
#include "prcrt.h"
#include "exec.h"
#include "perr.h"
#include "sock.h"

#define MAX_SOCKNO 32
static int sock_free;
int sock_used;

int ich_array[MAX_SOCKNO];
int och_array[MAX_SOCKNO];
int sock_next[MAX_SOCKNO];

static BOOL serv_array[MAX_SOCKNO];     // サーバソケットかどうかのチェック
static SOCKET sock_array[MAX_SOCKNO];
static WSAEVENT evt_array[MAX_SOCKNO];

int sock_inited;

/*
 * ソケットとチャネルの組を登録する
 */
static int sock_register(int ich, int och, SOCKET so) {
    WSAEVENT evt;
    int h;

    if ((h = sock_free) < 0) {
        assert(0);
        return -1;
    }

    if ((evt = WSACreateEvent()) == WSA_INVALID_EVENT) {
        perr(PERR_SYSTEM, "WSACreateEvent", WSAGetLastError(), __FILE__, __LINE__);
    }
    if (WSAEventSelect(so, evt, FD_READ|FD_WRITE|FD_CLOSE) == SOCKET_ERROR) {
        perr(PERR_SYSTEM, "WSAEventSelect", WSAGetLastError(), __FILE__, __LINE__);
    }

    ich_array[h]  = ich;
    och_array[h]  = och;
    sock_array[h] = so;
    evt_array[h]  = evt;
    serv_array[h] = FALSE;

    sock_free = sock_next[h];
    sock_next[h] = sock_used;
    sock_used = h;

    return h;
}

void prc_SockStart(void) {
    WSADATA Data;
    int status;
    int i;

    if ((status = WSAStartup(MAKEWORD(2,2),&Data)) != 0) {
        perr(PERR_SYSTEM, "WSAStartup", status, __FILE__, __LINE__);
    }

    /* ソケット配列の初期化 */
    for (i = 0; i < MAX_SOCKNO-1; i++) {
        sock_next[i] = i+1;
    }
    sock_next[i] = -1;
    sock_free = 0;
    sock_used = -1;

    sock_inited++;
}

void prc_SockFinish(void) {
    WSACleanup();
    sock_inited = 0;
}

/*
 * Udpクライアントソケットのハンドルを返す
 */
int prc_SockUdpClient(int ich, int och, char *host, int port) {
    struct hostent *serverHostent;
    SOCKADDR_IN addr;
    SOCKET sock;
    int status;
    int h;

    /* ハンドルのチェック */
    if ((h = sock_free) < 0) {
        assert(0);
        return -1;
    }
    serverHostent = gethostbyname(host);
    if (serverHostent == 0) {
        fprintf(stderr, "gethostbyname: failure\n");
        return -1;
    }
    memcpy(&addr.sin_addr, serverHostent->h_addr, serverHostent->h_length);
    addr.sin_port = htons(port);
    addr.sin_family = AF_INET;

    sock = socket(AF_INET, SOCK_DGRAM, 0);
    if (sock == INVALID_SOCKET) {
        fprintf(stderr, "socket: failure\n");
        return -1;
    }

    status = connect(sock, (LPSOCKADDR)&addr, sizeof(addr));
    if (status == SOCKET_ERROR) {
        fprintf(stderr, "connect: failure\n");
        closesocket(sock);
        return -1;
    }

    return sock_register(ich, och, sock);
}

/*
 * Udpクライアントソケットのハンドルを返す
 */
int prc_SockUdpOpen(int ich, int och, char *host, int cport, int bport) {
    struct hostent *serverHostent;
    SOCKADDR_IN addr;
    SOCKET sock;
    int status;
    int h;

    /* ハンドルのチェック */
    if ((h = sock_free) < 0) {
        assert(0);
        return -1;
    }
    serverHostent = gethostbyname(host);
    if (serverHostent == 0) {
        fprintf(stderr, "gethostbyname: failure\n");
        return -1;
    }
    sock = socket(AF_INET, SOCK_DGRAM, 0);
    if (sock == INVALID_SOCKET) {
        fprintf(stderr, "socket: failure\n");
        return -1;
    }

    addr.sin_port = htons(bport);
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = INADDR_ANY;
    status = bind(sock, (LPSOCKADDR)&addr, sizeof(addr));
    if (status == SOCKET_ERROR) {
        fprintf(stderr, "bind: failure\n");
        closesocket(sock);
        return -1;
    }

    memcpy(&addr.sin_addr, serverHostent->h_addr, serverHostent->h_length);
    addr.sin_port = htons(cport);
    addr.sin_family = AF_INET;
    status = connect(sock, (LPSOCKADDR)&addr, sizeof(addr));
    if (status == SOCKET_ERROR) {
        fprintf(stderr, "connect: failure\n");
        closesocket(sock);
        return -1;
    }

    return sock_register(ich, och, sock);
}


/*
 * クライアントソケットのハンドルを返す
 */
int prc_SockTcpClient(int ich, int och, char *host, int port) {
    struct hostent *serverHostent;
    SOCKADDR_IN addr;
    SOCKET sock;
    int status;
    int h;

    /* ハンドルのチェック */
    if ((h = sock_free) < 0) {
        return -1;
    }
    serverHostent = gethostbyname(host);
    if (serverHostent == 0) {
        fprintf(stderr, "gethostbyname: failure\n");
        return -1;
    }
    memcpy(&addr.sin_addr, serverHostent->h_addr, serverHostent->h_length);
    addr.sin_port = htons(port);
    addr.sin_family = AF_INET;

    sock = socket(AF_INET, SOCK_STREAM, 0);
    if (sock == INVALID_SOCKET) {
        fprintf(stderr, "socket: failure\n");
        return -1;
    }

    status = connect(sock, (LPSOCKADDR)&addr, sizeof(addr));
    if (status == SOCKET_ERROR) {
        fprintf(stderr, "connect: failure\n");
        closesocket(sock);
        return -1;
    }

    return sock_register(ich, och, sock);
}

/*
 * サーバーソケットのハンドルを返す
 */
int prc_SockTcpServer(int ch, int port) {
    SOCKADDR_IN addr;
    SOCKET sock;
    WSAEVENT evt;
    int status;
    int h;

    /* ハンドルのチェック */
    if ((h = sock_free) < 0) {
        return -1;
    }

    sock = socket(AF_INET, SOCK_STREAM, 0);
    if (sock == INVALID_SOCKET) {
        fprintf(stderr, "socket: failure\n");
        return -1;
    }
    addr.sin_port = htons(port);
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = INADDR_ANY;
    
    status = bind(sock, (LPSOCKADDR)&addr, sizeof(addr));
    if (status == SOCKET_ERROR) {
        fprintf(stderr, "bind: failure: %d\n", WSAGetLastError());
        closesocket(sock);
        return -1;
    }
    status = listen(sock, SOMAXCONN);
    if (status == SOCKET_ERROR) {
        fprintf(stderr, "listen: failure: %d\n", WSAGetLastError());
        closesocket(sock);
        return -1;
    }

    if ((evt = WSACreateEvent()) == WSA_INVALID_EVENT) {
        fprintf(stderr, "WSACreateEvent: failure\n");
        closesocket(sock);
        return -1;
    }
    if (WSAEventSelect(sock, evt, FD_ACCEPT) == SOCKET_ERROR) {
        fprintf(stderr, "WSAEventSelect: failure\n");
        closesocket(sock);
        WSACloseEvent(evt);
        return -1;
    }
    ich_array[h]  = ch;
    sock_array[h] = sock;
    evt_array[h]  = evt;
    serv_array[h] = TRUE;

    sock_free = sock_next[h];
    sock_next[h] = sock_used;
    sock_used = h;

    return h;
}

void prc_SockClose(int h) {
    int i;

    closesocket(sock_array[h]);
    WSACloseEvent(evt_array[h]);

    if (sock_used == h) {
        sock_used = sock_next[h];
    } else {
        for (i = sock_used; i >= 0; i = sock_next[i]) {
            if (h == sock_next[i]) {
                sock_next[i] = sock_next[h];
                break;
            }
        }
    }

    sock_next[h] = sock_free;
    sock_free = h;
}

extern ioent_t io_table[];
/* ソケット切断処理 */
static int func_sockdc(void) {
    ioent_t *io = &io_table[TOCINT(__prc__regs[1])];

    prc_SockClose(io->handle);
    __prc__regs[0] = __string__(0, "");
    return chan_send(ich_array[io->handle], __prc__regs[0]);
}

/* ソケット接続待機処理 */
static int func_sockac(void) {
    ioent_t *io = &io_table[TOCINT(__prc__regs[1])];
    SOCKET csock;
    SOCKADDR_IN addr;
    int len = sizeof(addr);

    if ((csock = accept(sock_array[io->handle], (LPSOCKADDR)&addr, &len)) == SOCKET_ERROR) {
        perr(PERR_SYSTEM, "accept", WSAGetLastError(), __FILE__, __LINE__);
    }
    __prc__regs[0] = (int)__chan__();
    __prc__regs[1] = (int)__chan__();
    sock_register(__prc__regs[0],__prc__regs[1], csock);
    __prc__regs[3] = __record__(2, __prc__regs[0], __prc__regs[1]);
    return chan_send(ich_array[io->handle], __prc__regs[3]);
}

/* ソケット入力処理 */
static int func_sockin(void) {
    static char buf[BUFSIZ];
    ioent_t *io = &io_table[TOCINT(__prc__regs[1])];
    int len;

    if ((len = recv(sock_array[io->handle], buf, BUFSIZ, 0)) == SOCKET_ERROR) {
        perr(PERR_SYSTEM, "recv", WSAGetLastError(), __FILE__, __LINE__);
    }
    if (!WSAResetEvent(evt_array[io->handle])) {
        perr(PERR_SYSTEM, "WSAResetEvent", WSAGetLastError(), __FILE__, __LINE__);
    }
    __prc__regs[0] = __string__(len, buf);
    return chan_send(ich_array[io->handle], __prc__regs[0]);
}

static int func_sockout(void) {
    ioent_t *io = &io_table[TOCINT(__prc__regs[1])];
    event_t *evt;
    int len;

    if ((evt = chout_next((chan_t *)och_array[io->handle])) == NULL) {
        // perr(PERR_INTERNAL, __FILE__, __LINE__);
        // Accept直後にWindows が空のFD_WRITEイベントをセットしてしまうことがある
        return (int)__disp__;
    }
    if ((len = STRLEN(evt->val)) == 0) {
        perr(PERR_INTERNAL, __FILE__, __LINE__);
    }
    if ((len = send(sock_array[io->handle], STRPTR(evt->val), len, 0)) == SOCKET_ERROR) {
        perr(PERR_SYSTEM, "send", GetLastError(), __FILE__, __LINE__);
    }
    if (!WSAResetEvent(evt_array[io->handle])) {
        perr(PERR_SYSTEM, "WSAResetEvent", WSAGetLastError(), __FILE__, __LINE__);
    }

    return chan_recv(och_array[io->handle]);
}

static int clos_sockac[1] = { (int)func_sockac };
static int clos_sockdc[1] = { (int)func_sockdc };
static int clos_sockin[1] = { (int)func_sockin };
static int clos_sockout[1] = { (int)func_sockout };

/* IOテーブルをセットする */
int sock_io(HANDLE handles[], ioent_t io_table[], int *io_count) {
    static char buf[BUFSIZ];
    int len;
    int i;

    if (!sock_inited) {
        return 0;
    }

    for (i = sock_used; i >= 0; i = sock_next[i]) {
        event_t *evt;
        SOCKET so = sock_array[i];
        ioent_t *io = &io_table[*io_count];
        int flag = 0;
        int ret;

        if (serv_array[i]) {
            if (chin_next((chan_t *)ich_array[i]) != NULL) {
                SOCKET csock;
                SOCKADDR_IN addr;
                int len = sizeof(addr);

                if ((csock = accept(so, (LPSOCKADDR)&addr, &len)) != SOCKET_ERROR) {
                    __prc__regs[0] = (int)__chan__();
                    __prc__regs[1] = (int)__chan__();
                    sock_register(__prc__regs[0],__prc__regs[1], csock);
                    __prc__regs[3] = __record__(2, __prc__regs[0], __prc__regs[1]);
                    return chan_send(ich_array[i], __prc__regs[3]);
                }
                if ((ret = WSAGetLastError()) != WSAEWOULDBLOCK) {
                    perr(PERR_SYSTEM, "accept", ret, __FILE__, __LINE__);
                }
                flag++;
            }
        } else {
            /* input channel */
            if (chin_next((chan_t *)ich_array[i]) != NULL) {
                if ((len = recv(so, buf, BUFSIZ, 0)) != SOCKET_ERROR) {
                    if (!WSAResetEvent(evt_array[i])) {
                        perr(PERR_SYSTEM, "WSAResetEvent", WSAGetLastError(), __FILE__, __LINE__);
                    }
                    __prc__regs[0] = __string__(len, buf);
                    return chan_send(ich_array[i], __prc__regs[0]);
                }
                if ((ret = WSAGetLastError()) != WSAEWOULDBLOCK) {
                    perr(PERR_SYSTEM, "recv", ret, __FILE__, __LINE__);
                }
                flag++;
            }
            /* output channel */
            if ((evt = chout_next((chan_t *)och_array[i])) != NULL) {
                len = STRLEN(evt->val);
                if (len == 0) {
                    prc_SockClose(i);
                    return chan_recv(och_array[i]);
                }
                if ((len = send(so, STRPTR(evt->val), len, 0)) != SOCKET_ERROR) {
                    if (!WSAResetEvent(evt_array[i])) {
                        perr(PERR_SYSTEM, "WSAResetEvent", WSAGetLastError(), __FILE__, __LINE__);
                    }
                    return chan_recv(och_array[i]);
                }
                if ((ret = WSAGetLastError()) != WSAEWOULDBLOCK) {
                    perr(PERR_SYSTEM, "send", ret, __FILE__, __LINE__);
                }
                fflush(stdout);
                flag++;
            }
        }
        if (flag) {
            handles[*io_count] = evt_array[i];
            io->type   = IOT_SOCK;
            io->handle = i;
            (*io_count)++;
        }
    }

    return  0;
}

/* 実行するクロージャを取り出す */
int sock_clos(int h) {
    WSANETWORKEVENTS NetworkEvents;

    if (WSAEnumNetworkEvents(sock_array[h], evt_array[h], &NetworkEvents) == SOCKET_ERROR) {
        perr(PERR_SYSTEM, "WSAEnumNetworkEvents", WSAGetLastError(), __FILE__, __LINE__);
    }
    if (NetworkEvents.lNetworkEvents & FD_READ) {
        return (int)clos_sockin;
    } else if (NetworkEvents.lNetworkEvents & FD_WRITE) {
        return (int)clos_sockout;
    } else if (NetworkEvents.lNetworkEvents & FD_ACCEPT) {
        return (int)clos_sockac;
    } else if (NetworkEvents.lNetworkEvents & FD_CLOSE) {
        return (int)clos_sockdc;
    }
    return __prc__disp;
}
