#define _FILE_OFFSET_BITS 64

#include <preccs/prcrt.h>
#include <stdio.h>
#include <arpa/inet.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/stat.h>
//#include <fcntl.h>
#include <errno.h>

#define BLOCK_SIZE 512
#define LEAST_BLOCK_NUM 1024

int g_fd;
int g_block_size = BLOCK_SIZE;
int g_block_num  = 0;

int terminate_fileIO();

static char *img_file = "./disk.img";
//static char *img_file = "./disk_10G.img";

/*
 * % iscsid [-s num] [image-file]
 * オプション：
 *  -s num     -- ディスクの容量をnum*ブロックサイズ(512bytes)分に設定する．
 *                省略時にはイメージファイルの大きさから自動的に計算される．
 *  image-file -- イメージファイルを指定する(省略時はdisk.img)
 *  
 */

int main(int argc, char *argv[]) {
    int c;
    int ret;

    while ((c = getopt(argc, argv, "s:")) >= 0) {
        switch (c) {
        case 's':
            g_block_num = atoi(optarg);
            if (g_block_num < LEAST_BLOCK_NUM) {
                printf("invalid block num(should be >= %d)\n", LEAST_BLOCK_NUM);
                return -1;
            }
            break;
        case '?':
            printf("unknown option\n");
            return -1;
        default:
            printf("?? getopt returned character code 0%o ??\n", c);
            return -1;
        }
    }
    if (optind < argc) {
        img_file = argv[optind++];
    }

    prc_SockStart();
    ret = prc_main();
    prc_SockFinish();

    terminate_fileIO();

    return ret;
}

void print_hex( char *title, char *msg, int size ) 
{
    int i;

    printf( "%s", title );

    for( i = 0; i < size; i++ ){
        if (i % 16 == 0)
            printf("\n");
        printf("%02x ", *(unsigned char *)(msg+i));
    }
    printf( "\n" );
}

int cast_to_int( char *msg, int size )
{
    int ret = 0;

    if( size == 1 ){
        ret = (int)*msg;
    } else if( size == 4) {
        ret = ntohl(*(int*)msg);
    }

    return ret;
}

int init_fileIO()
{
    struct stat st;

    g_fd = open( img_file , O_RDWR  );
    if( g_fd < 0 ) {
        printf( "Fail to open image file '%s': %d\n", img_file, errno );
        return -1;
    }
    if (stat( img_file, &st ) < 0) {
        perror("stat()");
        return -1;
    }
    printf("'%s' opened\n", img_file);
    printf("\t file size = %lld\n", st.st_size);
    printf("\t block num(in 512) = %lld\n", st.st_size/g_block_size);
    if (g_block_num == 0) {
        g_block_num = st.st_size/g_block_size;
    } else {
        printf("\t block num is specified to %d\n", g_block_num);
    }
    if (g_block_num < LEAST_BLOCK_NUM) {
        printf("invalid block num(should be >= %d)\n", LEAST_BLOCK_NUM);
        return -1;
    }
    return 0;
}

int terminate_fileIO()
{
    close( g_fd );

    return 0;
}

int read_data( off_t addr, size_t len, char* buf )
{
    int ret;
    lseek( g_fd, addr, SEEK_SET );
    ret = read(g_fd, buf, len );
    if( ret < 0 ) {
        printf( "Fail to read data : addr = %lld, len = %d\n", addr, len );
    }
    return 0;
}

int write_data( off_t addr, size_t len, char* buf )
{
    int ret;
    lseek( g_fd, addr, SEEK_SET );
    ret = write( g_fd, buf, len );
    if( ret < 0 ) {
        printf( "Fail to write data : addr = %lld, len = %d\n", addr, len );
    }
    //fsync( g_fd );
    return 0;
}
