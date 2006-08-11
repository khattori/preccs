#include <stdio.h>
#include <unistd.h>
#include "prcrt.h"
#include "global.h"
int  g_sport = 5060; 
int  g_cport = 5060;
int  g_rpport = 5004;
int  g_rmport = 5004;
char *g_host = "127.0.0.1";

static void display_help(void) {
    fprintf(stderr, "Usage: voip [-s sport] [-c cport] [-m rmport] [-p rpport] [-h host]\n");
}

int main(int argc, char *argv[]) {
    int c;
    int ret;

    while ((c = getopt(argc, argv, "s:c:m:p:h:")) != EOF) {
        switch (c) {
        case 's':
            g_sport = atoi(optarg);
            break;
        case 'c':
            g_cport = atoi(optarg);
            break;
        case 'm':
            g_rmport = atoi(optarg);
            break;
        case 'p':
            g_rpport = atoi(optarg);
            break;
        case 'h':
            g_host = optarg;
            break;
        case '?':
            display_help();
            return 0;
        }
    }
    prc_SockStart();
    ret = prc_main();
    prc_SockFinish();

    return ret;
}
