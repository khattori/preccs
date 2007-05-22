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
    ioent_t *ioin;
    ioent_t *ioout;
} ioevt_t;
#define evtin ioin->ctlblk.hEvent
#define evtout ioout->ctlblk.hEvent

static void so_input(ioent_t *io, event_t *evt, int exec);
static void so_output(ioent_t *io, event_t *evt, int exec);
static void so_accept(ioent_t *io, event_t *evt, int exec);
static void so_event(ioevt_t *io, event_t *evt, int exec);
static void send_exec(ioent_t *io, event_t *evt);

static ioevt_t *ioevt_create(SOCKET handle, ioent_t *ioin, ioent_t *ioout);
static void ioevt_delete(ioevt_t *io);


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
    ioent_t *ioin, *ioout;
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
    ioin = ioent_create((chan_t *)ich, (HANDLE)sock, IOT_INPUT, so_input, BUFSIZ);
    ioout = ioent_create((chan_t *)och, (HANDLE)sock, IOT_OUTPUT, so_output, BUFSIZ);
    ioevt_create(sock, ioin, ioout);

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

static void shutdown_recvsock(ioent_t *io) {
    ioevt_t *ioevt;
//    printf("shutdown_recvsock: enter\n");
//    fflush(stdout);

    if (shutdown((SOCKET)io->handle, SD_RECEIVE) == SOCKET_ERROR) {
        perr(PERR_SYSTEM, "shutdown", StrError(WSAGetLastError()), __FILE__, __LINE__);
        return ;
    }
    /*
     * すでにSO_SENDされている場合
     * ソケットクローズし，IOイベントも削除する
     */
    ioevt = io->data;
    if (ioevt->ioout == NULL) {
        closesocket((SOCKET)io->handle);
        ioevt_delete(ioevt);
    } else {
        ioevt->ioin = NULL;
    }

    /* 入力IOチャネルを削除 */
    ioent_delete(io);
}
static void shutdown_sendsock(ioent_t *io) {
    ioevt_t *ioevt;
//    printf("shutdown_sendsock: enter\n");
//    fflush(stdout);

    if (shutdown((SOCKET)io->handle, SD_SEND) == SOCKET_ERROR) {
        perr(PERR_SYSTEM, "shutdown", StrError(WSAGetLastError()), __FILE__, __LINE__);
        return ;
    }
    /*
     * すでにSO_RECVされている場合
     * ソケットクローズし，IOイベントも削除する
     */
    ioevt = io->data;
    if (ioevt->ioin == NULL) {
        closesocket((SOCKET)io->handle);
        ioevt_delete(ioevt);
    } else {
        ioevt->ioout = NULL;
    }

    /* 出力IOチャネルを削除 */
    ioent_delete(io);
}

static void CALLBACK recv_completion_handler(DWORD err, DWORD len, LPWSAOVERLAPPED ovl, DWORD flags) {
    event_t *evt;
    ioent_t *io = (ioent_t*)(((char*)ovl)-offsetof(ioent_t,ctlblk));

//    printf("recv completed: err=%ld, len=%ld\n", err, len);
//    fflush(stdout);

    aio_count--;

    io_read_complete(io, len);
    if (len == 0) {
        shutdown_recvsock(io);
        return;
    }
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

//    printf("so_input: enter(exec=%d, trans=%d)\n", exec, evt->trans);
//    fflush(stdout);
    if (evt->trans == 0) {
        aio_count++;
        /* 新規に受信IO発行 */
        wsabuf.len = io->bufsz;
        wsabuf.buf = io->buf;
        result = WSARecv((SOCKET)io->handle, &wsabuf, 1, (LPDWORD)&io->offset, &flags, &io->ctlblk, recv_completion_handler);
        if (result == 0) {
//            printf("so_input: WSARecv() result=0\n");
//            fflush(stdout);
            return;
        }
        error = WSAGetLastError();
        switch (error) {
        case WSA_IO_PENDING:
//            printf("so_input: WSARecv() WSA_IO_PENDING\n");
//            fflush(stdout);
            break;
        case WSAECONNRESET:
            aio_count--;
//            printf("so_input: WSARecv() WSAECONNRESET\n");
//            fflush(stdout);
            io_read_complete(io, 0);
            shutdown_recvsock(io);
            break;
        default:
            perr(PERR_SYSTEM, "WSARecv", StrError(error), __FILE__, __LINE__);
            break;
        }
        return;
    } else {
        ioevt_t *ioevt = (ioevt_t *)io->data;
        if (ioevt->mlink.tqe_prev == NULL) {
            TAILQ_INSERT_TAIL(&__prc__mioq, (ioent_t *)ioevt, mlink);
        }
    }
    TAILQ_INSERT_TAIL(&__prc__mioq, io, mlink);
}


static void CALLBACK send_completion_handler(DWORD err, DWORD len, LPWSAOVERLAPPED ovl, DWORD flags) {
    event_t *evt;
    ioent_t *io = (ioent_t*)(((char*)ovl)-offsetof(ioent_t,ctlblk));

//    printf("send completed: err=%ld, len=%ld\n", err, len);
//    fflush(stdout);

    aio_count--;
    io->offset += len;
    evt = (event_t*)chout_next(io->chan);
    if (io->offset < STRLEN(evt->val)) {
        send_exec(io, evt);
        return;
    }
    io_write_complete(io, len);
    /* IO待ちプロセスが無ければ終了 */
    if ((evt = chout_next(io->chan)) != NULL) {
        io->iof(io, evt, 0);
    }
}
static void send_exec(ioent_t *io, event_t *evt) {
    WSABUF wsabuf;
    DWORD flags = 0;
    int result;
    int error;
    int len;

    aio_count++;
    len = STRLEN(evt->val) - io->offset;
    if (len > io->bufsz) {
        len = io->bufsz;
    }
    /* 新規に送信IO発行 */
    memcpy(io->buf, STRPTR(evt->val)+io->offset, len);
    wsabuf.len = len;
    wsabuf.buf = io->buf;
    result = WSASend((SOCKET)io->handle, &wsabuf, 1, (LPDWORD)&len, flags, &io->ctlblk, send_completion_handler);
    if (result == 0) {
//            printf("so_output: WSASend() result=0\n");
//            fflush(stdout);
        return;
    }
    error = WSAGetLastError();
    switch (error) {
    case WSA_IO_PENDING:
//            printf("so_output: WSASend() WSA_IO_PENDING\n");
//            fflush(stdout);
        break;
    case WSAECONNRESET:
        aio_count--;
//            printf("so_input: WSASend() WSAECONNRESET\n");
//            fflush(stdout);
        io_write_complete(io, 0);
        shutdown_sendsock(io);
        break;
    default:
        perr(PERR_SYSTEM, "WSASend", StrError(error), __FILE__, __LINE__);
        break;
    }
}
/**
 * ソケットIOチャネル出力時の処理 
 */
static void so_output(ioent_t *io, event_t *evt, int exec) {
//    printf("so_output: enter(exec=%d,trans=%d)\n", exec, evt->trans);
//    fflush(stdout);

    if (evt->trans == 0) {
        int len = STRLEN(evt->val);
        if (len == 0) {
            io_write_complete(io, 0);
            shutdown_sendsock(io);
            return;
        }
        io->offset = 0;
        send_exec(io, evt);
        return;
    } else {
        ioevt_t *ioevt = (ioevt_t *)io->data;
        if (ioevt->mlink.tqe_prev == NULL) {
            TAILQ_INSERT_TAIL(&__prc__mioq, (ioent_t *)ioevt, mlink);
        }
    }
    TAILQ_INSERT_TAIL(&__prc__mioq, io, mlink);
}

static void accept_exec(ioent_t *io) {
    ioent_t *ioin, *ioout;
    proc_t *prc;
    event_t *evt;
    SOCKET so;

    if ((so = accept((SOCKET)io->handle, NULL, NULL)) == INVALID_SOCKET) {
	perr(PERR_SYSTEM, "accept", StrError(WSAGetLastError()), __FILE__, __LINE__);
        return;
    }
    __prc__regs[0] = (int)__chan__();
    __prc__regs[1] = (int)__chan__();
    ioin = ioent_create((chan_t *)__prc__regs[0], (HANDLE)so, IOT_INPUT, so_input, BUFSIZ);
    ioout = ioent_create((chan_t *)__prc__regs[1], (HANDLE)so, IOT_OUTPUT, so_output, BUFSIZ);
    ioevt_create(so, ioin, ioout);

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
static ioevt_t *ioevt_create(SOCKET handle, ioent_t *ioin, ioent_t *ioout) {
    WSAEVENT wsaevt;
    ioevt_t *io;

    io = malloc(sizeof(ioevt_t));
    if (io == NULL) {
        perr(PERR_OUTOFMEM, __FILE__, __LINE__);
        return NULL;
    }
    io->iotype = IOT_EVENT;
    io->iof    = (iof_t)so_event;
    io->chan   = NULL;
    io->handle = (HANDLE)handle;

    io->ioin   = ioin;
    io->ioout  = ioout;

    ioin->data = io;
    ioout->data = io;

    if ((wsaevt = WSACreateEvent()) == WSA_INVALID_EVENT) {
	perr(PERR_SYSTEM, "CreateEvent", StrError(WSAGetLastError()), __FILE__, __LINE__);
        return NULL;
    }
    if (WSAEventSelect(handle, wsaevt, FD_READ|FD_WRITE|FD_CLOSE) == SOCKET_ERROR) {
	perr(PERR_SYSTEM, "WSAEventSelect", StrError(WSAGetLastError()), __FILE__, __LINE__);
        return NULL;
    }
    memset(&io->ctlblk, 0, sizeof(OVERLAPPED));
    io->ctlblk.hEvent = wsaevt;

    TAILQ_INSERT_TAIL(&__prc__ioq, (ioent_t*)io, link);
    io->mlink.tqe_prev = NULL;
    // TAILQ_INSERT_TAIL(&__prc__mioq, (ioent_t*)io, mlink);

    return io;
}
/**
 * IOイベントエントリの削除
 */
static void ioevt_delete(ioevt_t *io) {
//    printf("ioevt_delete: enter\n");
//    fflush(stdout);

    TAILQ_REMOVE(&__prc__ioq, (ioent_t*)io, link);
    WSACloseEvent(io->ctlblk.hEvent);
    free(io);
}

/*
 * ソケットのイベント管理用
 */
static void so_event(ioevt_t *io, event_t *evt, int exec) {
    WSANETWORKEVENTS wsaevts;
    int result;
    int error;

//    printf("so_event: enter\n");
//    fflush(stdout);
    result = WSAEnumNetworkEvents((SOCKET)io->handle, io->ctlblk.hEvent, &wsaevts);
    if (result == SOCKET_ERROR) {
        error = WSAGetLastError();
        perr(PERR_SYSTEM, "WSAEnumNetworkEvents", StrError(error), __FILE__, __LINE__);
        return;
    }
    if (wsaevts.lNetworkEvents & FD_READ) {
//        printf("so_event: FD_READ\n");
//        fflush(stdout);

        if (!SetEvent(io->evtin)) {
            error = GetLastError();
            perr(PERR_SYSTEM, "SetEvent", StrError(error), __FILE__, __LINE__);
            return;
        }
    } else if (wsaevts.lNetworkEvents & FD_WRITE) {
//        printf("so_event: FD_WRITE\n");
//        fflush(stdout);

        if (!SetEvent(io->evtout)) {
            error = GetLastError();
            perr(PERR_SYSTEM, "SetEvent", StrError(error), __FILE__, __LINE__);
            return;
        }
    } else if (wsaevts.lNetworkEvents & FD_CLOSE) {
//        printf("so_event: FD_CLOSE\n");
//        fflush(stdout);

        if (!SetEvent(io->evtin)) {
            error = GetLastError();
            perr(PERR_SYSTEM, "SetEvent", StrError(error), __FILE__, __LINE__);
            return;
        }
        return;
    } else {
        /* 想定外のイベント */
        perr(PERR_INTERNAL, __FILE__, __LINE__);
        return;
    }
    if (((evt = chin_next(io->ioin->chan)) != NULL && evt->trans != 0) ||
        ((evt = chout_next(io->ioout->chan)) != NULL && evt->trans != 0)) {
        TAILQ_INSERT_TAIL(&__prc__mioq, (ioent_t *)io, mlink);
    }
}
