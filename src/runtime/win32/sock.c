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
#include "strerror.h"
#include "sock.h"

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
    ioent_create((chan_t *)ich, (HANDLE)sock, IO_TYPE_IN, BUFSIZ);
    ioent_create((chan_t *)och, (HANDLE)sock, IO_TYPE_OUT, BUFSIZ);

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
    ioent_create((chan_t *)ich, (HANDLE)sock, IO_TYPE_IN, BUFSIZ);
    ioent_create((chan_t *)och, (HANDLE)sock, IO_TYPE_OUT, BUFSIZ);

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

    ioent_create((chan_t *)ich, (HANDLE)sock, IO_TYPE_IN, BUFSIZ);
    ioent_create((chan_t *)och, (HANDLE)sock, IO_TYPE_OUT, BUFSIZ);

    return 0;
}

/*
 * サーバーソケットのハンドルを返す
 */
int prc_SockTcpServer(int ch, int port) {
    SOCKADDR_IN addr;
    SOCKET sock;
    BOOL option = TRUE;

    if ((sock = socket(AF_INET, SOCK_STREAM, 0)) == INVALID_SOCKET) {
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

    ioent_create((chan_t *)ch, (HANDLE)sock, IO_TYPE_ACCEPT, 0);

    return 0;
}

void prc_SockClose(int h) {
    perr(PERR_SYSTEM, "prc_SockClose", "not supported", __FILE__, __LINE__);
}
