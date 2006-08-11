/**
 * @file 
 * @brief �C�x���g�\���̒�`(���s�����C�u����)
 *
 *  �C�x���g�\���̂̓`���l���̓��o�̓L���[�̃G���g���ƂȂ�
 *
 * @author Kenta HATTORI
 * @date   2006/04/18
 * $Id: event.c,v 1.2 2006/06/21 00:13:06 hattori Exp $
 */
#include <stdlib.h>
#include "perr.h"
#include "event.h"
#include "gc.h"

/**
 * �C�x���g�I�u�W�F�N�g����
 */
event_t *event(int val, int clos, int trans) {
    event_t *evt;

    evt = (event_t *)gc_record(GC_ALIGN(sizeof(event_t)));
    evt->val   = (int)gc_forward((int*)val);
    evt->clos  = (int)gc_forward((int*)clos);
    evt->trans = (int)gc_forward((int*)trans);

    return evt;
}

/**
 * �C�x���g�I�u�W�F�N�g���
 */
/*
void evt_free(event_t *evt) {

}
*/

/**
 * �C�x���g�̃L�����Z������
 */
/*
void evt_cancel(event_t *evt) {
    ((int *)evt->trans)[0] = ~0;
}
*/
