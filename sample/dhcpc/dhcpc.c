#include <preccs/prcrt.h>

#include <stdio.h>
#include <unistd.h>
#include <net/if.h>
#include <netinet/in.h>
#include <sys/ioctl.h>

char *g_ifname;
char g_ifaddr[6];   /* MACアドレス */
char g_ipaddr[4];   /* クライアントIPアドレス */
char g_subnet[4];   /* サブネットマスク */
char g_gwaddr[4];   /* ゲートウェイアドレス */
char g_dnssrv[4];   /* DNSサーバアドレス */

#define DHCP_SERVER_PORT 67

static int get_ifaddr(char *ifname);

/*
 * DHCPクライアント
 */
int main(int argc, char *argv[]) {
    int ret;

    /* 引数のチェック */
    if (argc < 2) {
	fprintf(stderr, "usage: dhcpc ifname\n");
	return 1;
    }
    if (get_ifaddr(argv[1]) < 0) {
	return 1;
    }
    g_ifname = argv[1];

    prc_SockStart();
    ret = prc_main();
    prc_SockFinish();

    return ret;
}

/*
 * UDPソケットを作成
 */
int create_sock(void) {
    const int on = 1;
    struct sockaddr_in sockname;		/* ソケットのアドレス */
    int sock;

    /* UDPモードでソケットを作成 */
    if ((sock = socket(PF_INET, SOCK_DGRAM, 0)) < 0) {
	perror("socket");
	exit(1);
    }
    /* ブロードキャストモードに設定 */
    if (setsockopt(sock, SOL_SOCKET, SO_BROADCAST, (char *)&on, sizeof(on)) < 0) {
	perror("setsockopt");
	exit(1);
    }
    /* SO_REUSEADDRオプションをセット */
    if (setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, (char *)&on, sizeof(on)) < 0) {
	perror("setsockopt");
	exit(1);
    }
    /* ソケットのアドレスの構造体にサーバのIPアドレスとポート番号を設定 */
    sockname.sin_family      = AF_INET;
    sockname.sin_addr.s_addr = htonl(INADDR_ANY);
    sockname.sin_port        = htons(DHCP_SERVER_PORT+1); /* 受信ポート番号 */
    memset(sockname.sin_zero, (int)0, sizeof(sockname.sin_zero));
    if (bind(sock, (struct sockaddr *)&sockname, sizeof(sockname)) < 0) {
	perror("bind");
	exit(1);
    }
    /* ソケットのアドレスの構造体にサーバのIPアドレスとポート番号を設定 */
    sockname.sin_family      = AF_INET;
    sockname.sin_addr.s_addr = INADDR_BROADCAST;
    sockname.sin_port        = htons(DHCP_SERVER_PORT);	/* 送信ポート番号 */
    memset(sockname.sin_zero, (int)0, sizeof(sockname.sin_zero));
    if (connect(sock, (struct sockaddr *)&sockname, sizeof(sockname)) < 0) {
	perror("connect");
	exit(1);
    }

    return sock;
}

/*
 * ネットワークIFの設定
 */
int set_ifconf(void) {
    struct ifreq ifr;
    int s;
    
    if ((s = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
	perror("socket");
	return -1;
    }

    /* IPアドレス */
    strncpy(ifr.ifr_name, g_ifname, IFNAMSIZ - 1);
    ifr.ifr_addr.sa_family = AF_INET;
    memcpy(&((struct sockaddr_in *)&ifr.ifr_addr)->sin_addr, g_ipaddr, sizeof (struct in_addr));
    if (ioctl(s, SIOCSIFADDR, &ifr) < 0) {
	perror("ioctl");
	close(s);
	return -1;
    }
    /* サブネットマスク */
    memcpy(&((struct sockaddr_in *)&ifr.ifr_addr)->sin_addr, g_subnet, sizeof (struct in_addr));
    if (ioctl(s, SIOCSIFNETMASK, &ifr) < 0) {
	perror("ioctl");
	close(s);
	return -1;
    }

    close (s);

    return 0;

}

static int get_ifaddr(char *ifname) {
    int s;
    struct ifreq ifr;

    if ((s = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
	perror("socket");
	return -1;
    }

    ifr.ifr_addr.sa_family = AF_INET;
    strncpy(ifr.ifr_name, ifname, IFNAMSIZ - 1);
    if (ioctl(s, SIOCGIFHWADDR, &ifr) < 0) {
	perror("ioctl");
	close(s);
	return -1;
    }
    memcpy(g_ifaddr, ifr.ifr_hwaddr.sa_data, sizeof g_ifaddr);

    close(s);

    return 0;
}

