/**
 * @file 
 * @brief ソケット処理(実行時ライブラリ)
 *
 * @author Kenta HATTORI
 * @date   2006/04/26
 * $Id: sock.c,v 1.4 2006/08/07 09:07:51 hattori Exp $
 */
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <assert.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>

#include "prcrt.h"
#include "exec.h"
#include "perr.h"
#include "sock.h"

void prc_SockStart(void) {
}

void prc_SockFinish(void) {
}

/*
 * Udpクライアントソケットのハンドルを返す
 */
int prc_SockUdpClient(int ich, int och, char *host, int port) {
    struct hostent *hent;
    struct sockaddr_in addr;
    int sock;

    if ((hent = gethostbyname(host)) == NULL) {
        perr(PERR_SYSTEM, "gethostbyname", hstrerror(h_errno), __FILE__, __LINE__);
        return -1;
    }
    if ((sock = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
        perr(PERR_SYSTEM, "socket", strerror(errno), __FILE__, __LINE__);
        return -1;
    }

    memcpy(&addr.sin_addr, hent->h_addr, hent->h_length);
    addr.sin_port   = htons(port);
    addr.sin_family = AF_INET;
    if (connect(sock, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
        perr(PERR_SYSTEM, "connect", strerror(errno), __FILE__, __LINE__);
	close(sock);
        return -1;
    }
    ioent_create((chan_t *)ich, sock, IO_TYPE_IN, BUFSIZ);
    ioent_create((chan_t *)och, sock, IO_TYPE_OUT, BUFSIZ);

    return 0;
}

/*
 * Udpクライアントソケットのハンドルを返す
 */
int prc_SockUdpOpen(int ich, int och, char *host, int cport, int bport) {
    struct hostent *hent;
    struct sockaddr_in addr;
    int sock;

    if ((hent = gethostbyname(host)) == NULL) {
        perr(PERR_SYSTEM, "gethostbyname", strerror(errno), __FILE__, __LINE__);
        return -1;
    }
    if ((sock = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
        perr(PERR_SYSTEM, "socket", strerror(errno), __FILE__, __LINE__);
        return -1;
    }

    addr.sin_port = htons(bport);
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = INADDR_ANY;
    if (bind(sock, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
        perr(PERR_SYSTEM, "bind", strerror(errno), __FILE__, __LINE__);
	close(sock);
        return -1;
    }

    memcpy(&addr.sin_addr, hent->h_addr, hent->h_length);
    addr.sin_port = htons(cport);
    addr.sin_family = AF_INET;
    if (connect(sock, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
        perr(PERR_SYSTEM, "connect", strerror(errno), __FILE__, __LINE__);
	close(sock);
        return -1;
    }
    ioent_create((chan_t *)ich, sock, IO_TYPE_IN, BUFSIZ);
    ioent_create((chan_t *)och, sock, IO_TYPE_OUT, BUFSIZ);

    return 0;
}


/*
 * クライアントソケットのハンドルを返す
 */
int prc_SockTcpClient(int ich, int och, char *host, int port) {
    struct hostent *hent;
    struct sockaddr_in addr;
    int sock;

    if ((hent = gethostbyname(host)) == NULL) {
        perr(PERR_SYSTEM, "gethostbyname", strerror(errno), __FILE__, __LINE__);
        return -1;
    }
    if ((sock = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
        perr(PERR_SYSTEM, "socket", strerror(errno), __FILE__, __LINE__);
        return -1;
    }

    memcpy(&addr.sin_addr, hent->h_addr, hent->h_length);
    addr.sin_port = htons(port);
    addr.sin_family = AF_INET;
    if (connect(sock, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
        perr(PERR_SYSTEM, "connect", strerror(errno), __FILE__, __LINE__);
	close(sock);
        return -1;
    }

    ioent_create((chan_t *)ich, sock, IO_TYPE_IN, BUFSIZ);
    ioent_create((chan_t *)och, sock, IO_TYPE_OUT, BUFSIZ);

    return 0;
}

/*
 * サーバーソケットのハンドルを返す
 */
int prc_SockTcpServer(int ch, int port) {
    struct sockaddr_in addr;
    int sock;

    if ((sock = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
        perr(PERR_SYSTEM, "socket", strerror(errno), __FILE__, __LINE__);
        return -1;
    }
    /* TODO: fcntl()でO_NONBLOCKをセットする */

    addr.sin_port = htons(port);
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = INADDR_ANY;
    if (bind(sock, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
        perr(PERR_SYSTEM, "bind", strerror(errno), __FILE__, __LINE__);
	close(sock);
        return -1;
    }

    if (listen(sock, SOMAXCONN) < 0) {
        perr(PERR_SYSTEM, "listen", strerror(errno), __FILE__, __LINE__);
        close(sock);
        return -1;
    }

    ioent_create((chan_t *)ch, sock, IO_TYPE_ACCEPT, 0);

    return 0;
}

void prc_SockClose(int h) {
}
