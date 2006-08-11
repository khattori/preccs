#include "prcrt.h"
#include <stdio.h>
#include <time.h>

int main(void) {
    int ret;

    prc_SockStart();
    ret = prc_main();
    prc_SockFinish();

    return ret;
}

void print_ntp_time(char *msg) {
    time_t ntp_time;
    ntp_time = ntohl(*(int*)msg) - 2208988800U;
    printf("Now: %s\n", ctime(&ntp_time));
    fflush(stdout);
}
