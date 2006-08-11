/**
 * @file 
 * @brief ���顼��������(�¹Ի��饤�֥��)
 *
 * @author Kenta Hattori
 * @date   2005/07/05
 * $Id: perr.c,v 1.1 2006/05/25 06:53:51 hattori Exp $
 */
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "perr.h"

/*
 * ���顼���饹�����
 */
#define PERRCLS_FATAL 0
#define PERRCLS_ERROR 1
#define PERRCLS_WARN  2

/*
 * ���顼���������
 */
struct {
    int   cls;
    char *msg;
} errtbl[] = {
#define DEF_PERR(err, msg, cls) { cls, msg },
#include "perr.def"
#undef DEF_PERR
};

/**
 *  ���顼��å�������ɽ������
 *
 *  @param err [in] ���顼����
 *  @param ... [in] ���顼��å������Υѥ�᡼��
 *  @return �ʤ�
 *
 *  @note FATAL�ξ���exit(1)���롣
 *
 */
void perr(perr_t err, ...) {
    va_list ap;
    int cls   = errtbl[err].cls;
    char *msg = errtbl[err].msg;

    va_start(ap, err);
    vfprintf(stderr, msg, ap);
    fprintf(stderr, "\n");
    fflush(stderr);
    va_end(ap);

    /* FATAL���Ͻ�λ */
    if (cls == PERRCLS_FATAL) {
	exit(1);
    }
}

