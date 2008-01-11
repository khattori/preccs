/**
 * @file 
 * @brief ソケット処理(実行時ライブラリ)
 *
 * @author Kenta HATTORI
 * @date   2006/04/26
 * $Id: sock.h,v 1.4 2006/08/07 09:07:51 hattori Exp $
 */
#ifndef __INC_SOCK_H__
#define __INC_SOCK_H__

void prc_SockStart(void);
void prc_SockFinish(void);

void prc_SockSetConnChan(int ich, int och, int so);
void prc_SockSetUnconnChan(int ich, int och, int so);
int prc_SockTcpServer(int ch, int port);
int prc_SockTcpClient(int ich, int och, char *host, int port);
int prc_SockUdpServer(int ich, int och, int port);
int prc_SockUdpClient(int ich, int och, char *host, int port);
int prc_SockUdpOpen(int ich, int och, char *host, int cport, int bport);
void prc_SockClose(int h);

#ifdef WIN32
int sock_io(HANDLE handles[], ioent_t io_table[], int *io_count);
#else
int sock_io(int handles[], ioent_t io_table[], int *io_count);
#endif

int sock_clos(int h);

#endif /* __INC_SOCK_H__ */
