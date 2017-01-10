#include "Rts.h"
#include "Stable.h"
#include "TopHandler.h"

#ifdef THREADED_RTS
static Mutex m; // Protects the operations on topHandlerPtr,
                // which aren't atomic
#endif
static StgStablePtr topHandlerPtr;

void rts_setMainThread(StgWeak *weak) {
    ACQUIRE_LOCK(&m);
    if (topHandlerPtr != NULL) {
        freeStablePtr(topHandlerPtr); // OK to do under the lock
    }
    topHandlerPtr = getStablePtr((StgPtr)weak);
    // referent is a Weak#
    ASSERT(weak->header.info == &stg_WEAK_info);

    // See Note [rts_setMainThread has an unsound type] in
    // libraries/base/GHC/TopHandler.hs.
    ASSERT(weak->key->header.info == &stg_TSO_info);

    RELEASE_LOCK(&m);
}

StgTSO *getTopHandlerThread(void) {
    ACQUIRE_LOCK(&m);
    StgWeak *weak = (StgWeak*)deRefStablePtr(topHandlerPtr);
    RELEASE_LOCK(&m);
    const StgInfoTable *info = weak->header.info;
    if (info == &stg_WEAK_info) {
        StgClosure *key = ((StgWeak*)weak)->key;

        // See Note [rts_setMainThread has an unsound type] in
        // libraries/base/GHC/TopHandler.hs.
        ASSERT(key->header.info == &stg_TSO_info);

        return (StgTSO *)key;
    } else if (info == &stg_DEAD_WEAK_info) {
        return NULL;
    } else {
        barf("getTopHandlerThread: neither a WEAK nor a DEAD_WEAK: %p %p %d",
             weak, info, info->type);
        return NULL;
    }
}

void initTopHandler(void) {
#ifdef THREADED_RTS
    initMutex(&m);
#endif
    topHandlerPtr = NULL;
}

void exitTopHandler(void) {
    freeStablePtr(topHandlerPtr);
    topHandlerPtr = NULL;
#ifdef THREADED_RTS
    closeMutex(&m);
#endif
}
