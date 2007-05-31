#include <preccs/prcrt.h>
#include <stdio.h>
#include <unistd.h>
#include <getopt.h>
#include "iotest.h"

int g_test_mode;
static struct option long_options[] = {
    { "stdin",  0, &g_test_mode, MODE_STDIN  },
    { "stdout", 0, &g_test_mode, MODE_STDOUT },
    { "timer",  0, &g_test_mode, MODE_TIMER  },
    { "server", 0, &g_test_mode, MODE_SERVER },
    { "client", 0, &g_test_mode, MODE_CLIENT },
    { "help", 0, 0, 0 },
    { 0, 0, 0, 0 }
};
static void print_usage(char *prog) {
    printf("Usage: %s mode\n", prog);
    printf("  -i, --stdin     run test program in stdin mode\n");
    printf("  -o, --stdout    run test program in stdout mode\n");
    printf("  -t, --timer     run test program in timer mode\n");
    printf("  -s, --server    run test program in server mode\n");
    printf("  -c, --client    run test program in client mode\n");
    printf("  -h, --help      display this message\n");
}

int main(int argc, char *argv[]) {
    int c;

    c = getopt_long_only(argc, argv, "iotsch", long_options, NULL);
    switch (c) {
    case 0:
	break;
    case 'i':	g_test_mode = MODE_STDIN;	break;
    case 'o':	g_test_mode = MODE_STDOUT;	break;
    case 't':	g_test_mode = MODE_TIMER;	break;
    case 's':	g_test_mode = MODE_SERVER;	break;
    case 'c':	g_test_mode = MODE_CLIENT;	break;
    case 'h': case -1:
	print_usage(argv[0]);
	return 0;
    default:
	printf("?? getopt returned character code 0%o ??\n", c);
	return -1;
    }
    // printf("g_test_mode=%d\n", g_test_mode);
    return prc_main();
}
