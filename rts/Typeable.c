#include "RtsTypeable.h"
#include "Rts.h"

static StgPtr typeableStore = 0;
#ifdef THREADED_RTS
Mutex typeableStoreLock;
#endif


void
initTypeableStore()
{
    typeableStore=0;
#ifdef THREADED_RTS
    initMutex(&typeableStoreLock);
#endif
}

void
exitTypeableStore()
{
#ifdef THREADED_RTS
    closeMutex(&typeableStoreLock);
#endif
    if(typeableStore!=0) {
        freeStablePtr((StgStablePtr)typeableStore);
        typeableStore=0;
    }
}

StgPtr
getOrSetTypeableStore(StgPtr ptr)
{
    StgPtr ret = typeableStore;
    if(ret==0) {
#ifdef THREADED_RTS
        ACQUIRE_LOCK(&typeableStoreLock);
        ret=typeableStore;
        if(ret==0) {
#endif
            typeableStore = ret = ptr;
#ifdef THREADED_RTS
        }
        RELEASE_LOCK(&typeableStoreLock);
#endif
    }
    return ret;
}
