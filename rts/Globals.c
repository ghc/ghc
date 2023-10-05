/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1995-2009
 *
 * The RTS stores some "global" values on behalf of libraries, so that
 * some libraries can ensure that certain top-level things are shared
 * even when multiple versions of the library are loaded.  e.g. see
 * Data.Typeable and GHC.Conc.
 *
 * How are multiple versions of a library loaded? Examples:
 *
 *   base - a statically-linked ghci has its own copy, so might libraries it
 *          dynamically loads
 *
 *   libHSghc - a statically-linked ghc has its own copy and so will Core
 *              plugins it dynamically loads.
 *
 * ---------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"

#include "Globals.h"
#include "StablePtr.h"

typedef enum {
    GHCConcSignalSignalHandlerStore,
    GHCConcWindowsPendingDelaysStore,
    GHCConcWindowsIOManagerThreadStore,
    GHCConcWindowsProddingStore,
    SystemEventThreadEventManagerStore,
    SystemEventThreadIOManagerThreadStore,
    SystemTimerThreadEventManagerStore,
    SystemTimerThreadIOManagerThreadStore,
    LibHSghcFastStringTable,
    LibHSghcGlobalHasPprDebug,
    LibHSghcGlobalHasNoDebugOutput,
    LibHSghcGlobalHasNoStateHack,
    MaxStoreKey
} StoreKey;

#if defined(THREADED_RTS)
Mutex globalStoreLock;
#endif

static StgStablePtr store[MaxStoreKey];

void
initGlobalStore(void)
{
    uint32_t i;
    for (i=0; i < MaxStoreKey; i++) {
        store[i] = 0;
    }
#if defined(THREADED_RTS)
    initMutex(&globalStoreLock);
#endif
}

void
exitGlobalStore(void)
{
    uint32_t i;
#if defined(THREADED_RTS)
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
#if defined(THREADED_RTS)
        ACQUIRE_LOCK(&globalStoreLock);
        ret = store[key];
        if(ret==0) {
#endif
            store[key] = ret = ptr;
#if defined(THREADED_RTS)
        }
        RELEASE_LOCK(&globalStoreLock);
#endif
    }
    return ret;
}

#define mkStoreAccessor(name) \
    StgStablePtr \
    getOrSet##name(StgStablePtr ptr) \
    { return getOrSetKey(name, ptr); }

mkStoreAccessor(GHCConcSignalSignalHandlerStore)
mkStoreAccessor(GHCConcWindowsPendingDelaysStore)
mkStoreAccessor(GHCConcWindowsIOManagerThreadStore)
mkStoreAccessor(GHCConcWindowsProddingStore)
mkStoreAccessor(SystemEventThreadEventManagerStore)
mkStoreAccessor(SystemEventThreadIOManagerThreadStore)
mkStoreAccessor(SystemTimerThreadEventManagerStore)
mkStoreAccessor(SystemTimerThreadIOManagerThreadStore)
mkStoreAccessor(LibHSghcFastStringTable)
mkStoreAccessor(LibHSghcGlobalHasPprDebug)
mkStoreAccessor(LibHSghcGlobalHasNoDebugOutput)
mkStoreAccessor(LibHSghcGlobalHasNoStateHack)

HsWord64 ghc_unique_counter64 = 0;
HsInt ghc_unique_inc     = 1;
