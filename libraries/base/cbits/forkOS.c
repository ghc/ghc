/* 
 * (c) The GHC Team 2003
 *
 * $Id: forkOS.c,v 1.1 2003/09/21 22:20:57 wolfgang Exp $
 *
 * Helper function for Control.Concurrent.forkOS
 */

#include "HsBase.h"
#include "RtsAPI.h"

#if defined(HAVE_PTHREAD_H) && !defined(WANT_NATIVE_WIN32_THREADS)
#include <pthread.h>

static void *
forkOS_createThreadWrapper ( void * entry )
{
    rts_lock();
    rts_evalStableIO((HsStablePtr) entry, NULL);
    rts_unlock();
    return NULL;
}

int
forkOS_createThread ( HsStablePtr entry )
{
    pthread_t tid;
    int result = pthread_create(&tid, NULL,
				forkOS_createThreadWrapper, (void*)entry);
    if(!result)
        pthread_detach(tid);
    return result;
}

#elif defined(HAVE_WINDOWS_H)
#include <windows.h>

static unsigned __stdcall
forkOS_createThreadWrapper ( void * entry )
{
    rts_lock();
    rts_evalStableIO((HsStablePtr) entry, NULL);
    rts_unlock();
    return 0;
}

int
forkOS_createThread ( HsStablePtr entry )
{
    return (_beginthreadex ( NULL,  /* default security attributes */
			   0,
			   forkOS_createThreadWrapper,
			   (void*)entry,
			   0,
			   (unsigned*)pId) == 0);
}

#else
#endif