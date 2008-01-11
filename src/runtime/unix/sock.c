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
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netdb.h>

#include "prcrt.h"
#include "proc.h"
#include "exec.h"
#include "perr.h"
#include "sock.h"

extern void write_exec(ioent_t *io, event_t *evt);
extern void io_complete(ioent_t * io);
static void so_input(ioent_t *io, event_t *evt, int exec);
static void so_output(ioent_t *io, event_t *evt, int exec);
static void so_accept(ioent_t *io, event_t *evt, int exec);
static void so_sendto(ioent_t *io, event_t *evt, int exec);
static void so_recvfrom(ioent_t *io, event_t *evt, int exec);

void prc_SockStart(void) {
}

void prc_SockFinish(void) {
}

/*
 * ソケットをセットしチャネルを返す
 */
void prc_SockSetUnconnChan(int ich, int och, int so) {
    ioent_t *iie, *oie;

    iie = ioent_create((chan_t *)ich, so, IOT_INPUT, so_recvfrom, BUFSIZ);
    oie = ioent_create((chan_t *)och, so, IOT_OUTPUT, so_sendto, BUFSIZ);
    iie->data = oie;
    oie->data = iie;
}
/*
 * ソケットをセットしチャネルを返す
 */
void prc_SockSetConnChan(int ich, int och, int so) {
    ioent_t *iie, *oie;

    iie = ioent_create((chan_t *)ich, so, IOT_INPUT, so_input, BUFSIZ);
    oie = ioent_create((chan_t *)och, so, IOT_OUTPUT, so_output, BUFSIZ);
    iie->data = oie;
    oie->data = iie;
}

/*
 * Udpサーバソケットのハンドルを返す
 */
int prc_SockUdpServer(int ich, int och, int port) {
    struct sockaddr_in addr;
    int sock;

    if ((sock = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
        perr(PERR_SYSTEM, "socket", strerror(errno), __FILE__, __LINE__);
        return -1;
    }
    addr.sin_port = htons(port);
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = INADDR_ANY;
    if (bind(sock, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
        perr(PERR_SYSTEM, "bind", strerror(errno), __FILE__, __LINE__);
	close(sock);
        return -1;
    }
    prc_SockSetUnconnChan(ich, och, sock);

    return 0;
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
    prc_SockSetConnChan(ich, och, sock);

    return 0;
}

/*
 * Udp接続済みソケットのハンドルを返す
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
    prc_SockSetConnChan(ich, och, sock);

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
        perr(PERR_SYSTEM, "gethostbyname", hstrerror(h_errno), __FILE__, __LINE__);
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
    prc_SockSetConnChan(ich, och, sock);

    return 0;
}

/*
 * サーバーソケットのハンドルを返す
 */
int prc_SockTcpServer(int ch, int port) {
    struct sockaddr_in addr;
    int option = 1;
    int sock;

    if ((sock = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
        perr(PERR_SYSTEM, "socket", strerror(errno), __FILE__, __LINE__);
        return -1;
    }
    if (setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, &option, sizeof(option)) < 0) {
        perr(PERR_SYSTEM, "setsockopt", strerror(errno), __FILE__, __LINE__);
        return -1;
    }
    if (setsockopt(sock, IPPROTO_TCP, TCP_NODELAY, &option, sizeof(option)) < 0) {
        perr(PERR_SYSTEM, "setsockopt", strerror(errno), __FILE__, __LINE__);
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

    ioent_create((chan_t *)ch, sock, IOT_INPUT, so_accept, 0);

    return 0;
}

void prc_SockClose(int h) {
    perr(PERR_SYSTEM, "prc_SockClose", "not supported", __FILE__, __LINE__);
}

static void accept_exec(ioent_t *io) {
    proc_t *prc;
    event_t *evt;
    int so;

    if ((so = accept(io->handle, NULL, NULL)) < 0) {
	if (errno == EMFILE) {
	    extern sigset_t ss_sigio;
	    siginfo_t si;
	    perr(PWRN_SYSTEM, "accept", strerror(errno), __FILE__, __LINE__);
	    /* IO完了シグナルを処理 */
	    while (aio_count > 0) {
	    	if (sigwaitinfo(&ss_sigio, &si) != SIGRTMIN+SIGIO) {
	    	    perr(PERR_SYSTEM, "sigwaitinfo", strerror(errno), __FILE__, __LINE__);
		    return;
	    	}
	        io_complete((ioent_t *)si.si_value.sival_ptr);
	    }
    	    TAILQ_INSERT_TAIL(&__prc__mioq, io, mlink);
	    return;
	}
	perr(PERR_SYSTEM, "accept", strerror(errno), __FILE__, __LINE__);
	return;
    }
    __prc__regs[0] = (int)__chan__();
    __prc__regs[3] = (int)__chan__();
    prc_SockSetConnChan(__prc__regs[0], __prc__regs[3], so);
    __prc__regs[2] = __record__(2);
    ((int*)__prc__regs[2])[0] = __prc__regs[0];
    ((int*)__prc__regs[2])[1] = __prc__regs[3];

    /* 実行可能プロセスをセット */
    prc = proc();
    evt = chin_next(io->chan);
    prc->clos = evt->clos;
    prc->val  = __prc__regs[2];
    TAILQ_INSERT_TAIL(__prc__rdyq, prc, link);
    TAILQ_REMOVE(&io->chan->inq, evt, link);

    /* IO待ちプロセスが無ければ終了 */
    if ((evt = chin_next(io->chan)) != NULL) {
	io->iof(io, evt, 0);
    }
}
/**
 * IOチャネル入力時の処理
 */
static void so_input(ioent_t *io, event_t *evt, int exec) {
    if (evt->trans == 0) {
        aio_count++;
        io->ctlblk.aio_offset = 0;
        if (aio_read(&io->ctlblk) < 0) {
            perr(PERR_SYSTEM, "aio_read", strerror(errno), __FILE__, __LINE__);
        }
        return;
    }
    TAILQ_INSERT_TAIL(&__prc__mioq, io, mlink);
}

/**
 * ソケットIOチャネル出力時の処理 
 */
static void so_output(ioent_t *io, event_t *evt, int exec) {
    if (evt->trans == 0) {
        int len = STRLEN(evt->val);
        if (len == 0) {
            io_write_complete(io, 0);
	    if (io->data) {
                shutdown(io->handle, SHUT_WR);
		((ioent_t *)io->data)->data = NULL;
	    } else {
 		close(io->handle);
	    }
	    ioent_delete(io);
            return;
        }
        io->offset = 0;
        io->ctlblk.aio_offset = 0;
        write_exec(io, evt);
	return;
    }
    TAILQ_INSERT_TAIL(&__prc__mioq, io, mlink);
}

static void sendto_exec(ioent_t *io, event_t *evt) {
    struct sockaddr_in addr;
    int len;

    addr.sin_port = htons(TOCINT(RCDIDX(RCDIDX(evt->val,1),1)));
    addr.sin_family = AF_INET;
    memcpy(&addr.sin_addr.s_addr, STRPTR(RCDIDX(RCDIDX(evt->val,1),0)), sizeof(addr.sin_addr.s_addr));

    len = STRLEN(RCDIDX(evt->val,0)) - io->offset;
    if (len > io->bufsz) {
	len = io->bufsz;
    }
    /* 新規に書き込みIO発行 */
    memcpy(io->buf, STRPTR(RCDIDX(evt->val,0))+io->offset, len);
    if ((len = sendto(io->handle, io->buf, len, 0, (struct sockaddr *)&addr, sizeof addr)) < 0) {
	perr(PERR_SYSTEM, "sendto", strerror(errno), __FILE__, __LINE__);
        return;
    }
    io->offset += len;
    if (io->offset < STRLEN(RCDIDX(evt->val,0))) {
        sendto_exec(io, evt);
        return;
    }
    io_write_complete(io, STRLEN(RCDIDX(evt->val,0)));
    if ((evt = chout_next(io->chan)) != NULL) {
 	io->iof(io, evt, 0);
    }
}

/**
 * ソケットIOチャネル出力時(sendto)の処理 
 */
static void so_sendto(ioent_t *io, event_t *evt, int exec) {
    if (evt->trans == 0) {
        int len = STRLEN(RCDIDX(evt->val,0));

        if (len == 0) {
            io_write_complete(io, 0);
	    if (io->data) {
                shutdown(io->handle, SHUT_WR);
		((ioent_t *)io->data)->data = NULL;	
	    } else {
		close(io->handle);
	    }
	    ioent_delete(io);
            return;
        }
        io->offset = 0;
        sendto_exec(io, evt);
	return;
    }
    TAILQ_INSERT_TAIL(&__prc__mioq, io, mlink);
}

static void so_recvfrom_complete(ioent_t *io, int len, in_addr_t addr, int port) {
    proc_t *prc;
    event_t *evt;

    __prc__regs[0] = __string__(len, (void *)io->buf);
    __prc__regs[4] = __string__(sizeof addr, (void *)&addr);
    __prc__regs[3] = __record__(2);
    __prc__regs[2] = __record__(2);
    ((int*)__prc__regs[2])[0] = __prc__regs[0];
    ((int*)__prc__regs[2])[1] = __prc__regs[3];
    memcpy(STRPTR(__prc__regs[4]), &addr, sizeof addr);
    ((int*)__prc__regs[3])[0] = __prc__regs[4];
    ((int*)__prc__regs[3])[1] = TOPINT(port);
    prc = proc();
    evt = chin_next(io->chan);
    prc->clos = evt->clos;
    prc->val  = __prc__regs[2];
    TAILQ_INSERT_TAIL(__prc__rdyq, prc, link);
    TAILQ_REMOVE(&io->chan->inq, evt, link);
}

/**
 * ソケットIOチャネル入力時(recvfrom)の処理 
 */
static void so_recvfrom(ioent_t *io, event_t *evt, int exec) {
    if (exec) {
        struct sockaddr_in addr;
        socklen_t fromlen = sizeof addr;
        int len;

        if ((len = recvfrom(io->handle, io->buf, io->bufsz, 0, (struct sockaddr *)&addr, &fromlen)) < 0) {
	    perr(PERR_SYSTEM, "sendto", strerror(errno), __FILE__, __LINE__);
            return;
        }
	so_recvfrom_complete(io, len, addr.sin_addr.s_addr, ntohs(addr.sin_port));
	if ((evt = chin_next(io->chan)) != NULL) {
	    io->iof(io, evt, 0);
	}
	return;
    }
    TAILQ_INSERT_TAIL(&__prc__mioq, io, mlink);
}

static void so_accept(ioent_t *io, event_t *evt, int exec) {
    if (exec) {
	accept_exec(io);
	return;
    }
    TAILQ_INSERT_TAIL(&__prc__mioq, io, mlink);
}
