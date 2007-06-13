#include <preccs/prcrt.h>
#include <stdio.h>

int main(void) {
    return prc_main();
}

int padding(int len) {
    return (4-len%4)%4;
}
