/**
 * @file 
 * @brief ファイル入出力処理(実行時ライブラリ)
 *
 * @author Kenta HATTORI
 * @date   2006/04/26
 * $Id: file.h,v 1.2 2006/08/07 09:07:51 hattori Exp $
 */
#ifndef __INC_FILE_H__
#define __INC_FILE_H__

void file_init(void);
int file_io(HANDLE handles[], ioent_t io_table[], int *io_count);
int file_clos(int h);

int prc_FileOpenR(int ich, char *fname);
int prc_FileCreate(int och, char *fname);

#endif /* __INC_FILE_H__ */
