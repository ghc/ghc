/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1995-2009
 *
 * The RTS stores some "global" values on behalf of libraries, so that
 * some libraries can ensure that certain top-level things are shared
 * even when multiple versions of the library are loaded.  e.g. see
 * Data.Typeable and GHC.Conc.
 *
 * If/when we switch to a dynamically-linked GHCi, this can all go
 * away, because there would be just one copy of each library.
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "Globals.h"
#include "Stable.h"

typedef enum {
    GHCConcSignalSignalHandlerStore,
    GHCConcWindowsPendingDelaysStore,
    GHCConcWindowsIOManagerThreadStore,
    GHCConcWindowsProddingStore,
    SystemEventThreadEventManagerStore,
    SystemEventThreadIOManagerThreadStore,
    SystemTimerThreadEventManagerStore,
    SystemTimerThreadIOManagerThreadStore,
    MaxStoreKey
} StoreKey;

#ifdef THREADED_RTS
Mutex globalStoreLock;
#endif

static StgStablePtr store[MaxStoreKey];

void
initGlobalStore(void)
{
    nat i;
    for (i=0; i < MaxStoreKey; i++) {
        store[i] = 0;
    }
#ifdef THREADED_RTS
    initMutex(&globalStoreLock);
#endif
}

void
exitGlobalStore(void)
{
    nat i;
#ifdef THREADED_RTS
    closeMutex(&globalStoreLock);
#endif
    for (i=0; i < MaxStoreKey; i++) {
        if (store[i] != 0) {
            freeStablePtr(store[i]);
            store[i] = 0;
        }
    }
}

static StgStablePtr getOrSetKey(StoreKey key, StgStablePtr ptr)
{
    StgStablePtr ret = store[key];
    if(ret==0) {
#ifdef THREADED_RTS
        ACQUIRE_LOCK(&globalStoreLock);
        ret = store[key];
        if(ret==0) {
#endif
            store[key] = ret = ptr;
#ifdef THREADED_RTS
        }
        RELEASE_LOCK(&globalStoreLock);
#endif
    }
    return ret;
}    

StgStablePtr
getOrSetGHCConcSignalSignalHandlerStore(StgStablePtr ptr)
{
    return getOrSetKey(GHCConcSignalSignalHandlerStore,ptr);
}

StgStablePtr
getOrSetGHCConcWindowsPendingDelaysStore(StgStablePtr ptr)
{
    return getOrSetKey(GHCConcWindowsPendingDelaysStore,ptr);
}

StgStablePtr
getOrSetGHCConcWindowsIOManagerThreadStore(StgStablePtr ptr)
{
    return getOrSetKey(GHCConcWindowsIOManagerThreadStore,ptr);
}

StgStablePtr
getOrSetGHCConcWindowsProddingStore(StgStablePtr ptr)
{
    return getOrSetKey(GHCConcWindowsProddingStore,ptr);
}

StgStablePtr
getOrSetSystemEventThreadEventManagerStore(StgStablePtr ptr)
{
    return getOrSetKey(SystemEventThreadEventManagerStore,ptr);
}

StgStablePtr
getOrSetSystemEventThreadIOManagerThreadStore(StgStablePtr ptr)
{
    return getOrSetKey(SystemEventThreadIOManagerThreadStore,ptr);
}

StgStablePtr
getOrSetSystemTimerThreadEventManagerStore(StgStablePtr ptr)
{
    return getOrSetKey(SystemTimerThreadEventManagerStore,ptr);
}

StgStablePtr
getOrSetSystemTimerThreadIOManagerThreadStore(StgStablePtr ptr)
{
    return getOrSetKey(SystemTimerThreadIOManagerThreadStore,ptr);
}
