#include <preccs/prcrt.h>

int main(void) {
    int ret;

    prc_SockStart();
    ret = prc_main();
    prc_SockFinish();

    return ret;
}

