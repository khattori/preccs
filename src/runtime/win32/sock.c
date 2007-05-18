/**
 * @file 
 * @brief ソケット処理(実行時ライブラリ)
 *
 * @author Kenta HATTORI
 * @date   2006/04/26
 * $Id: sock.c,v 1.4 2006/08/07 09:07:51 hattori Exp $
 */
#include <stdio.h>
#include <stddef.h>
#include <assert.h>
#include <winsock2.h>
#include "prcrt.h"
#include "proc.h"
#include "exec.h"
#include "perr.h"
#include "strerror.h"
#include "sock.h"

/*
 * WSAEVENT用のIO構造体(ioent_t互換)
 */
typedef struct ioevt_ {
    TAILQ_ENTRY(ioent_) link;  /* リンク */
    TAILQ_ENTRY(ioent_) mlink; /* 多重IO用リンク */
    iotype_t iotype;           /* IO種別 */
    iof_t iof;                 /* IO処理関数へのポインタ */
    chan_t *chan;              /* チャネルへのリンク */
    HANDLE handle;
    ioctlblk_t ctlblk;
    HANDLE evtin;
    HANDLE evtout;
} ioevt_t;

static void so_input(ioent_t *io, event_t *evt, int exec);
static void so_output(ioent_t *io, event_t *evt, int exec);
static void so_accept(ioent_t *io, event_t *evt, int exec);
static void so_event(ioevt_t *io, event_t *evt, int exec);

static void ioevt_create(SOCKET handle, HANDLE evtin, HANDLE evtout);

void prc_SockStart(void) {
    WORD version = MAKEWORD(2, 2);
    WSADATA wsadata;
    int status;

    if ((status = WSAStartup(version, &wsadata)) != 0) {
        perr(PERR_SYSTEM, "WSAStartup", StrError(status), __FILE__, __LINE__);
    }
}

void prc_SockFinish(void) {
    WSACleanup();
}

/*
 * Udpクライアントソケットのハンドルを返す
 */
int prc_SockUdpClient(int ich, int och, char *host, int port) {
    struct hostent *hent;
    SOCKADDR_IN addr;
    SOCKET sock;

    if ((hent = gethostbyname(host)) == NULL) {
        perr(PERR_SYSTEM, "gethostbyname", StrError(WSAGetLastError()), __FILE__, __LINE__);
        return -1;
    }
    if ((sock = socket(AF_INET, SOCK_DGRAM, 0)) == INVALID_SOCKET) {
        perr(PERR_SYSTEM, "socket", StrError(WSAGetLastError()), __FILE__, __LINE__);
        return -1;
    }

    memcpy(&addr.sin_addr, hent->h_addr, hent->h_length);
    addr.sin_port = htons(port);
    addr.sin_family = AF_INET;
    if (connect(sock, (LPSOCKADDR)&addr, sizeof(addr)) == SOCKET_ERROR) {
        perr(PERR_SYSTEM, "connect", StrError(WSAGetLastError()), __FILE__, __LINE__);
        closesocket(sock);
        return -1;
    }
    ioent_create((chan_t *)ich, (HANDLE)sock, IOT_INPUT, so_input, BUFSIZ);
    ioent_create((chan_t *)och, (HANDLE)sock, IOT_OUTPUT, so_output, BUFSIZ);

    return 0;
}

/*
 * Udpクライアントソケットのハンドルを返す
 */
int prc_SockUdpOpen(int ich, int och, char *host, int cport, int bport) {
    struct hostent *hent;
    SOCKADDR_IN addr;
    SOCKET sock;

    if ((hent = gethostbyname(host)) == NULL) {
        perr(PERR_SYSTEM, "gethostbyname", StrError(WSAGetLastError()), __FILE__, __LINE__);
        return -1;
    }
    if ((sock = socket(AF_INET, SOCK_DGRAM, 0)) == INVALID_SOCKET) {
        perr(PERR_SYSTEM, "socket", StrError(WSAGetLastError()), __FILE__, __LINE__);
        return -1;
    }

    addr.sin_port = htons(bport);
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = INADDR_ANY;
    if (bind(sock, (struct sockaddr *)&addr, sizeof(addr)) == SOCKET_ERROR) {
        perr(PERR_SYSTEM, "bind", StrError(WSAGetLastError()), __FILE__, __LINE__);
	closesocket(sock);
        return -1;
    }

    memcpy(&addr.sin_addr, hent->h_addr, hent->h_length);
    addr.sin_port = htons(cport);
    addr.sin_family = AF_INET;
    if (connect(sock, (struct sockaddr *)&addr, sizeof(addr)) == SOCKET_ERROR) {
        perr(PERR_SYSTEM, "connect", StrError(WSAGetLastError()), __FILE__, __LINE__);
	closesocket(sock);
        return -1;
    }
    ioent_create((chan_t *)ich, (HANDLE)sock, IOT_INPUT, so_input, BUFSIZ);
    ioent_create((chan_t *)och, (HANDLE)sock, IOT_OUTPUT, so_output, BUFSIZ);

    return 0;
}

/*
 * クライアントソケットのハンドルを返す
 */
int prc_SockTcpClient(int ich, int och, char *host, int port) {
    struct hostent *hent;
    SOCKADDR_IN addr;
    SOCKET sock;

    if ((hent = gethostbyname(host)) == NULL) {
        perr(PERR_SYSTEM, "gethostbyname", StrError(WSAGetLastError()), __FILE__, __LINE__);
        return -1;
    }
    if ((sock = socket(AF_INET, SOCK_STREAM, 0)) == INVALID_SOCKET) {
        perr(PERR_SYSTEM, "socket", StrError(WSAGetLastError()), __FILE__, __LINE__);
        return -1;
    }

    memcpy(&addr.sin_addr, hent->h_addr, hent->h_length);
    addr.sin_port = htons(port);
    addr.sin_family = AF_INET;
    if (connect(sock, (struct sockaddr *)&addr, sizeof(addr)) == SOCKET_ERROR) {
        perr(PERR_SYSTEM, "connect", StrError(WSAGetLastError()), __FILE__, __LINE__);
	closesocket(sock);
        return -1;
    }
    ioent_create((chan_t *)ich, (HANDLE)sock, IOT_INPUT, so_input, BUFSIZ);
    ioent_create((chan_t *)och, (HANDLE)sock, IOT_OUTPUT, so_output, BUFSIZ);
    ioevt_create(sock, ((chan_t *)ich)->ioent->ctlblk.hEvent, ((chan_t *)och)->ioent->ctlblk.hEvent);

    return 0;
}

/*
 * サーバーソケットのハンドルを返す
 */
int prc_SockTcpServer(int ch, int port) {
    SOCKADDR_IN addr;
    SOCKET sock;
    BOOL option = TRUE;

    if ((sock = WSASocket(AF_INET, SOCK_STREAM, IPPROTO_IP, NULL, 0, WSA_FLAG_OVERLAPPED)) == INVALID_SOCKET) {
        perr(PERR_SYSTEM, "socket", StrError(WSAGetLastError()), __FILE__, __LINE__);
        return -1;
    }
    if (setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, (char *)&option, sizeof(option)) == SOCKET_ERROR) {
        perr(PERR_SYSTEM, "setsockopt", StrError(WSAGetLastError()), __FILE__, __LINE__);
        return -1;
    }
    /* TODO: fcntl()でO_NONBLOCKをセットする */
    addr.sin_port = htons(port);
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = INADDR_ANY;
    if (bind(sock, (struct sockaddr *)&addr, sizeof(addr)) == SOCKET_ERROR) {
        perr(PERR_SYSTEM, "bind", StrError(WSAGetLastError()), __FILE__, __LINE__);
	closesocket(sock);
        return -1;
    }

    if (listen(sock, SOMAXCONN) == SOCKET_ERROR) {
        perr(PERR_SYSTEM, "listen", StrError(WSAGetLastError()), __FILE__, __LINE__);
        closesocket(sock);
        return -1;
    }

    ioent_create((chan_t *)ch, (HANDLE)sock, IOT_INPUT, so_accept, 0);
    if (WSAEventSelect(sock, ((chan_t *)ch)->ioent->ctlblk.hEvent, FD_ACCEPT) == SOCKET_ERROR) {
        perr(PERR_SYSTEM, "WSAEventSelect", StrError(WSAGetLastError()), __FILE__, __LINE__);
        closesocket(sock);
        return -1;
    }

    return 0;
}

void prc_SockClose(int h) {
    perr(PERR_SYSTEM, "prc_SockClose", "not supported", __FILE__, __LINE__);
}

static void CALLBACK recv_completion_handler(DWORD err, DWORD len, LPWSAOVERLAPPED ovl, DWORD flags) {
    event_t *evt;
    ioent_t *io = (ioent_t*)(((char*)ovl)-offsetof(ioent_t,ctlblk));

    printf("recv completed: err=%ld, len=%ld\n", err, len);
    fflush(stdout);

    aio_count--;
    io_read_complete(io, len);
    /* IO待ちプロセスが無ければ終了 */
    if ((evt = chin_next(io->chan)) == NULL) {
        return;
    }
    io->iof(io, evt, 0);
}
/**
 * ソケットIOチャネル入力時の処理 
 */
static void so_input(ioent_t *io, event_t *evt, int exec) {
    WSABUF wsabuf;
    DWORD flags = 0;
    int result;
    int error;

    printf("so_input: enter(exec=%d, trans=%d)\n", exec, evt->trans);
    fflush(stdout);
    if (evt->trans == 0) {
        aio_count++;
        /* 新規に受信IO発行 */
        wsabuf.len = io->bufsz;
        wsabuf.buf = io->buf;
        result = WSARecv((SOCKET)io->handle, &wsabuf, 1, (LPDWORD)&io->offset, &flags, &io->ctlblk, recv_completion_handler);
        if (result == 0) {
            printf("so_input: WSARend() result=0\n");
            fflush(stdout);

            return;
        }
        error = WSAGetLastError();
        switch (error) {
        case WSA_IO_PENDING:
            printf("so_input: WSARecv() WSA_IO_PENDING\n");
            fflush(stdout);
            break;
        case WSAECONNRESET:
            aio_count--;
            io_read_complete(io, 0);
            break;
        default:
            perr(PERR_SYSTEM, "WSARecv", StrError(error), __FILE__, __LINE__);
            break;
        }
        return;
    }
    TAILQ_INSERT_TAIL(&__prc__mioq, io, mlink);
}

static void CALLBACK send_completion_handler(DWORD err, DWORD len, LPWSAOVERLAPPED ovl, DWORD flags) {
    event_t *evt;
    ioent_t *io = (ioent_t*)(((char*)ovl)-offsetof(ioent_t,ctlblk));

    printf("send completed: err=%ld, len=%ld\n", err, len);
    fflush(stdout);

    aio_count--;
    io_write_complete(io, len);
    /* IO待ちプロセスが無ければ終了 */
    if ((evt = chout_next(io->chan)) == NULL) {
        return;
    }
    io->iof(io, evt, 0);
}
/**
 * ソケットIOチャネル出力時の処理 
 */
static void so_output(ioent_t *io, event_t *evt, int exec) {
    WSABUF wsabuf;
    DWORD flags = 0;
    int result;
    int error;

    printf("so_output: enter(exec=%d,trans=%d)\n", exec, evt->trans);
    fflush(stdout);

    if (evt->trans == 0) {
        aio_count++;
        /* 新規に送信IO発行 */
        /* TODO:
          * outstandingなIOが残っている場合に
          * GCが発生する可能性を考慮し
          * バッファにコピーする
          */
        wsabuf.len = STRLEN(evt->val);
        wsabuf.buf = STRPTR(evt->val);
        result = WSASend((SOCKET)io->handle, &wsabuf, 1, (LPDWORD)&io->offset, flags, &io->ctlblk, send_completion_handler);
        if (result == 0) {
            printf("so_output: WSASend() result=0\n");
            fflush(stdout);
            return;
        }
        error = WSAGetLastError();
        switch (error) {
        case WSA_IO_PENDING:
            printf("so_output: WSASend() WSA_IO_PENDING\n");
            fflush(stdout);
            break;
        default:
            perr(PERR_SYSTEM, "WSASend", StrError(error), __FILE__, __LINE__);
            break;
        }
        return;
    }
    TAILQ_INSERT_TAIL(&__prc__mioq, io, mlink);
}

static void accept_exec(ioent_t *io) {
    proc_t *prc;
    event_t *evt;
    SOCKET so;

    if ((so = accept((SOCKET)io->handle, NULL, NULL)) == INVALID_SOCKET) {
	perr(PERR_SYSTEM, "accept", StrError(WSAGetLastError()), __FILE__, __LINE__);
        return;
    }
    __prc__regs[0] = (int)__chan__();
    __prc__regs[1] = (int)__chan__();
    ioent_create((chan_t *)__prc__regs[0], (HANDLE)so, IOT_INPUT, so_input, BUFSIZ);
    ioent_create((chan_t *)__prc__regs[1], (HANDLE)so, IOT_OUTPUT, so_output, BUFSIZ);
    ioevt_create(so, ((chan_t *)__prc__regs[0])->ioent->ctlblk.hEvent, ((chan_t *)__prc__regs[1])->ioent->ctlblk.hEvent);

    __prc__regs[2] = __record__(2);
    ((int*)__prc__regs[2])[0] = __prc__regs[0];
    ((int*)__prc__regs[2])[1] = __prc__regs[1];

    /* 実行可能プロセスをセット */
    prc = proc();
    evt = chin_next(io->chan);
    prc->clos = evt->clos;
    prc->val  = __prc__regs[2];
    TAILQ_INSERT_TAIL(__prc__rdyq, prc, link);
    TAILQ_REMOVE(&io->chan->inq, evt, link);

    /* IO待ちプロセスが無ければ終了 */
    if ((evt = chin_next(io->chan)) == NULL) {
        return;
    }
    io->iof(io, evt, 0);
}
/**
 * Listen IOチャネル入力時の処理
 */
static void so_accept(ioent_t *io, event_t *evt, int exec) {
    if (exec) {
        accept_exec(io);
        return;
    }

    TAILQ_INSERT_TAIL(&__prc__mioq, io, mlink);
}

/**
 * IOイベントエントリの新規作成
 */
static void ioevt_create(SOCKET handle, HANDLE evtin, HANDLE evtout) {
    WSAEVENT wsaevt;
    ioevt_t *io;

    io = malloc(sizeof(ioevt_t));
    if (io == NULL) {
        perr(PERR_OUTOFMEM, __FILE__, __LINE__);
        return;
    }
    io->iotype = IOT_EVENT;
    io->iof    = (iof_t)so_event;
    io->chan   = NULL;
    io->handle = (HANDLE)handle;
    io->evtin  = evtin;
    io->evtout = evtout;

    if ((wsaevt = WSACreateEvent()) == WSA_INVALID_EVENT) {
	perr(PERR_SYSTEM, "CreateEvent", StrError(WSAGetLastError()), __FILE__, __LINE__);
        return;
    }
    if (WSAEventSelect(handle, wsaevt, FD_READ|FD_WRITE|FD_CLOSE) == SOCKET_ERROR) {
	perr(PERR_SYSTEM, "WSAEventSelect", StrError(WSAGetLastError()), __FILE__, __LINE__);
        return;
    }
    memset(&io->ctlblk, 0, sizeof(OVERLAPPED));
    io->ctlblk.hEvent = wsaevt;

    TAILQ_INSERT_TAIL(&__prc__ioq, (ioent_t*)io, link);
    TAILQ_INSERT_TAIL(&__prc__mioq, (ioent_t*)io, mlink);
}

/*
 * ソケットのイベント管理用
 */
static void so_event(ioevt_t *io, event_t *evt, int exec) {
    WSANETWORKEVENTS wsaevts;
    int result;
    int error;

    printf("so_event: enter\n");
    fflush(stdout);
    result = WSAEnumNetworkEvents((SOCKET)io->handle, io->ctlblk.hEvent, &wsaevts);
    if (result == SOCKET_ERROR) {
        error = WSAGetLastError();
        perr(PERR_SYSTEM, "WSAEnumNetworkEvents", StrError(error), __FILE__, __LINE__);
        return;
    }
    if (wsaevts.lNetworkEvents & FD_READ) {
        printf("so_event: FD_READ\n");
        fflush(stdout);

        if (!SetEvent(io->evtin)) {
            error = GetLastError();
            perr(PERR_SYSTEM, "SetEvent", StrError(error), __FILE__, __LINE__);
            return;
        }
    } else if (wsaevts.lNetworkEvents & FD_WRITE) {
        printf("so_event: FD_WRITE\n");
        fflush(stdout);

        if (!SetEvent(io->evtout)) {
            error = GetLastError();
            perr(PERR_SYSTEM, "SetEvent", StrError(error), __FILE__, __LINE__);
            return;
        }
    } else if (wsaevts.lNetworkEvents & FD_CLOSE) {
        printf("so_event: FD_CLOSE\n");
        fflush(stdout);

        if (!SetEvent(io->evtin)) {
            error = GetLastError();
            perr(PERR_SYSTEM, "SetEvent", StrError(error), __FILE__, __LINE__);
            return;
        }
    } else {
        /* 想定外のイベント */
        perr(PERR_INTERNAL, __FILE__, __LINE__);
        return;
    }

    TAILQ_INSERT_TAIL(&__prc__mioq, (ioent_t *)io, mlink);
}
