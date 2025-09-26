#include "Rts.h"
#include "Prelude.h"
#include "BuiltinClosures.h"

/*
 * Note [CHARLIKE and INTLIKE closures]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * These are static representations of Chars and small Ints, so that
 * we can remove dynamic Chars and Ints during garbage collection and
 * replace them with references to the static objects.
 */

StgIntCharlikeClosure stg_INTLIKE_closure[MAX_INTLIKE - MIN_INTLIKE + 1];
StgIntCharlikeClosure stg_CHARLIKE_closure[MAX_CHARLIKE - MIN_CHARLIKE + 1];

void initBuiltinClosures() {
    // INTLIKE closures
    for (int i = MIN_INTLIKE; i <= MAX_INTLIKE; i++) {
        StgIntCharlikeClosure *c = &stg_INTLIKE_closure[i - MIN_INTLIKE];
        SET_HDR((StgClosure* ) c, Izh_con_info, CCS_SYSTEM_OR_NULL);
        c->data = i;
    }

    // CHARLIKE closures
    for (int i = MIN_CHARLIKE; i <= MAX_CHARLIKE; i++) {
        StgIntCharlikeClosure *c = &stg_CHARLIKE_closure[i - MIN_CHARLIKE];
        SET_HDR((StgClosure* ) c, Czh_con_info, CCS_SYSTEM_OR_NULL);
        c->data = i;
    }
}
