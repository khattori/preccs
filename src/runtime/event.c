/**
 * @file 
 * @brief イベント構造体定義(実行時ライブラリ)
 *
 *  イベント構造体はチャネルの入出力キューのエントリとなる
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
 * イベントオブジェクト生成
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
 * イベントオブジェクト解放
 */
/*
void evt_free(event_t *evt) {

}
*/

/**
 * イベントのキャンセル処理
 */
/*
void evt_cancel(event_t *evt) {
    ((int *)evt->trans)[0] = ~0;
}
*/
