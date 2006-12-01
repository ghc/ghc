/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2006
 *
 * The IO manager thread in THREADED_RTS.  
 * See also libraries/base/GHC/Conc.lhs.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "ThrIOManager.h"
#include "Prelude.h"
#include <windows.h>

// Here's the Event that we use to wake up the IO manager thread
static HANDLE io_manager_event = INVALID_HANDLE_VALUE;

// must agree with values in GHC.Conc:
#define IO_MANAGER_WAKEUP 0xffffffff
#define IO_MANAGER_DIE    0xfffffffe
// spurios wakeups are returned as zero.
// console events are ((event<<1) | 1)

HANDLE
getIOManagerEvent (void)
{
    // This function has to exist even in the non-THREADED_RTS,
    // because code in GHC.Conc refers to it.  It won't ever be called
    // unless we're in the threaded RTS, however.
#ifdef THREADED_RTS
    HANDLE hRes;

    if (io_manager_event == INVALID_HANDLE_VALUE) {
        hRes = CreateEvent ( NULL, // no security attrs
                             TRUE, // manual reset
                             FALSE, // initial state,
                             "IO Manager Event" );
        if (hRes == NULL) {
            sysErrorBelch("getIOManagerEvent");
            stg_exit(EXIT_FAILURE);
        }
        io_manager_event = hRes;
        return hRes;
    } else {
        return io_manager_event;
    }
#else
    return NULL;
#endif
}


#if defined(THREADED_RTS)

#define EVENT_BUFSIZ 256
Mutex event_buf_mutex;
StgWord32 event_buf[EVENT_BUFSIZ];
nat next_event;

#endif

StgWord32
readIOManagerEvent (void)
{
    // This function must exist even in non-THREADED_RTS, 
    // see getIOManagerEvent() above.
#if defined(THREADED_RTS)
    StgWord32 res;

    ACQUIRE_LOCK(&event_buf_mutex);
    if (io_manager_event != INVALID_HANDLE_VALUE) {
        if (next_event == 0) {
            res = 0; // no event to return
        } else {
            res = event_buf[--next_event];
            if (next_event == 0) {
                if (!ResetEvent(io_manager_event)) {
                    sysErrorBelch("readIOManagerEvent");
                    stg_exit(EXIT_FAILURE);
                }
            }
        }
    } else {
        res = 0;
    }
    RELEASE_LOCK(&event_buf_mutex);
    // debugBelch("readIOManagerEvent: %d\n", res);
    return res;
#else
    return 0;
#endif
}

void
sendIOManagerEvent (StgWord32 event)
{
#if defined(THREADED_RTS)
    // debugBelch("sendIOManagerEvent: %d\n", event);
    ACQUIRE_LOCK(&event_buf_mutex);
    if (io_manager_event != INVALID_HANDLE_VALUE) {
        if (next_event == EVENT_BUFSIZ) {
            errorBelch("event buffer overflowed; event dropped");
        } else {
            if (!SetEvent(io_manager_event)) {
                sysErrorBelch("sendIOManagerEvent");
                stg_exit(EXIT_FAILURE);
            }        
            event_buf[next_event++] = event;
        }
    }
    RELEASE_LOCK(&event_buf_mutex);
#endif
}    

#if defined(THREADED_RTS)
void
ioManagerWakeup (void)
{
    sendIOManagerEvent(IO_MANAGER_WAKEUP);
}

void
ioManagerDie (void)
{
    sendIOManagerEvent(IO_MANAGER_DIE);
    // ToDo: wait for the IO manager to pick up the event, and
    // then release the Event and Mutex objects we've allocated.
}

void
ioManagerStart (void)
{
    initMutex(&event_buf_mutex);
    next_event = 0;

    // Make sure the IO manager thread is running
    Capability *cap;
    if (io_manager_event == INVALID_HANDLE_VALUE) {
	cap = rts_lock();
	rts_evalIO(cap,&base_GHCziConc_ensureIOManagerIsRunning_closure,NULL);
	rts_unlock(cap);
    }
}
#endif
