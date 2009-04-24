/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1995-2009
 *
 * The RTS stores some "global" values on behalf of libraries, so that
 * some libraries can ensure that certain top-level things are shared
 * even when multiple versions of the library are loaded.  e.g. see
 * Data.Typeable and GHC.Conc.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "RtsGlobals.h"

static StgStablePtr typeableStore      = 0;
static StgStablePtr signalHandlerStore = 0;

#ifdef THREADED_RTS
Mutex globalStoreLock;
#endif

void
initGlobalStore(void)
{
    typeableStore      = 0;
    signalHandlerStore = 0;
#ifdef THREADED_RTS
    initMutex(&globalStoreLock);
#endif
}

void
exitGlobalStore(void)
{
#ifdef THREADED_RTS
    closeMutex(&globalStoreLock);
#endif
    if(typeableStore!=0) {
        freeStablePtr((StgStablePtr)typeableStore);
        typeableStore=0;
    }
    if(signalHandlerStore!=0) {
        freeStablePtr((StgStablePtr)signalHandlerStore);
        signalHandlerStore=0;
    }
}

StgStablePtr
getOrSetTypeableStore(StgStablePtr ptr)
{
    StgStablePtr ret = typeableStore;
    if(ret==0) {
#ifdef THREADED_RTS
        ACQUIRE_LOCK(&globalStoreLock);
        ret=typeableStore;
        if(ret==0) {
#endif
            typeableStore = ret = ptr;
#ifdef THREADED_RTS
        }
        RELEASE_LOCK(&globalStoreLock);
#endif
    }
    return ret;
}

StgStablePtr
getOrSetSignalHandlerStore(StgStablePtr ptr)
{
    StgStablePtr ret = signalHandlerStore;
    if(ret==0) {
#ifdef THREADED_RTS
        ACQUIRE_LOCK(&globalStoreLock);
        ret=signalHandlerStore;
        if(ret==0) {
#endif
            signalHandlerStore = ret = ptr;
#ifdef THREADED_RTS
        }
        RELEASE_LOCK(&globalStoreLock);
#endif
    }
    return ret;
}
