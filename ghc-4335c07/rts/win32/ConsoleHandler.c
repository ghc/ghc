/*
 * Console control handler support.
 *
 */
#include "Rts.h"
#include <windows.h>
#include "ConsoleHandler.h"
#include "Schedule.h"
#include "RtsUtils.h"
#include "AsyncIO.h"
#include "RtsSignals.h"

extern int stg_InstallConsoleEvent(int action, StgStablePtr *handler);

static BOOL WINAPI shutdown_handler(DWORD dwCtrlType);
static BOOL WINAPI generic_handler(DWORD dwCtrlType);

static bool deliver_event = true;
StgInt console_handler = STG_SIG_DFL;

#if !defined(THREADED_RTS)

static HANDLE hConsoleEvent = INVALID_HANDLE_VALUE;

#define N_PENDING_EVENTS 16
StgInt stg_pending_events = 0;           /* number of undelivered events */
DWORD stg_pending_buf[N_PENDING_EVENTS]; /* their associated event numbers. */

#endif

/*
 * Function: initUserSignals()
 *
 * Initialize the console handling substrate.
 */
void
initUserSignals(void)
{
    console_handler = STG_SIG_DFL;
#if !defined (THREADED_RTS)
    stg_pending_events = 0;
    if (hConsoleEvent == INVALID_HANDLE_VALUE) {
        hConsoleEvent =
            CreateEvent ( NULL,  /* default security attributes */
                          TRUE,  /* manual-reset event */
                          FALSE, /* initially non-signalled */
                          NULL); /* no name */
    }
#endif
    return;
}

void
freeSignalHandlers(void) {
    /* Do nothing */
}

/* Seems to be a bit of an orphan...where used? */
void
finiUserSignals(void)
{
#if !defined (THREADED_RTS)
    if (hConsoleEvent != INVALID_HANDLE_VALUE) {
        CloseHandle(hConsoleEvent);
    }
#endif
}

/*
 * Function: shutdown_handler()
 *
 * Local function that performs the default handling of Ctrl+C kind
 * events; gently shutting down the RTS
 *
 * To repeat Signals.c remark -- user code may choose to override the
 * default handler. Which is fine, assuming they put back the default
 * handler when/if they de-install the custom handler.
 *
 */
static BOOL WINAPI shutdown_handler(DWORD dwCtrlType)
{
    switch (dwCtrlType) {

    case CTRL_CLOSE_EVENT:
        /* see generic_handler() comment re: this event */
        return false;
    case CTRL_C_EVENT:
    case CTRL_BREAK_EVENT:

        // If we're already trying to interrupt the RTS, terminate with
        // extreme prejudice.  So the first ^C tries to exit the program
        // cleanly, and the second one just kills it.
        if (sched_state >= SCHED_INTERRUPTING) {
            stg_exit(EXIT_INTERRUPTED);
        } else {
            interruptStgRts();
        }
        return true;

        /* shutdown + logoff events are not handled here. */
    default:
        return false;
    }
}


/*
 * Function: initDefaultHandlers()
 *
 * Install any default signal/console handlers. Currently we install a
 * Ctrl+C handler that shuts down the RTS in an orderly manner.
 */
void initDefaultHandlers(void)
{
    if ( !SetConsoleCtrlHandler(shutdown_handler, true) ) {
        errorBelch("warning: failed to install default console handler");
    }
}

void resetDefaultHandlers(void)
{
    if ( !SetConsoleCtrlHandler(shutdown_handler, false) ) {
        errorBelch("warning: failed to uninstall default console handler");
    }
}

/*
 * Function: blockUserSignals()
 *
 * Temporarily block the delivery of further console events. Needed to
 * avoid race conditions when GCing the stack of outstanding handlers or
 * when emptying the stack by running the handlers.
 *
 */
void
blockUserSignals(void)
{
    deliver_event = false;
}


/*
 * Function: unblockUserSignals()
 *
 * The inverse of blockUserSignals(); re-enable the deliver of console events.
 */
void
unblockUserSignals(void)
{
    deliver_event = true;
}


/*
 * Function: awaitUserSignals()
 *
 * Wait for the next console event. Currently a NOP (returns immediately.)
 */
void awaitUserSignals(void)
{
    return;
}


#if !defined (THREADED_RTS)
/*
 * Function: startSignalHandlers()
 *
 * Run the handlers associated with the stacked up console events. Console
 * event delivery is blocked for the duration of this call.
 */
void startSignalHandlers(Capability *cap)
{
    StgStablePtr handler;

    if (console_handler < 0) {
        return;
    }

    blockUserSignals();
    ACQUIRE_LOCK(&sched_mutex);

    handler = deRefStablePtr((StgStablePtr)console_handler);
    while (stg_pending_events > 0) {
        stg_pending_events--;
        StgTSO *t =
            createIOThread(cap,
                           RtsFlags.GcFlags.initialStkSize,
                           rts_apply(cap,
                                     (StgClosure *)handler,
                                     rts_mkInt(cap,
                                               stg_pending_buf[stg_pending_events])));
        scheduleThread(cap, t);
        labelThread(cap, t, "signal handler thread");
    }

    RELEASE_LOCK(&sched_mutex);
    unblockUserSignals();
}
#endif /* !THREADED_RTS */

/*
 * Function: markSignalHandlers()
 *
 * Evacuate the handler stack. _Assumes_ that console event delivery
 * has already been blocked.
 */
void markSignalHandlers (evac_fn evac STG_UNUSED, void *user STG_UNUSED)
{
    // nothing to mark; the console handler is a StablePtr which is
    // already treated as a root by the GC.
}


/*
 * Function: generic_handler()
 *
 * Local function which handles incoming console event (done in a separate
 * OS thread), recording the event in stg_pending_events.
 */
static BOOL WINAPI generic_handler(DWORD dwCtrlType)
{
    /* Ultra-simple -- up the counter + signal a switch. */
    switch(dwCtrlType) {
    case CTRL_CLOSE_EVENT:
        /* Don't support the delivery of this event; if we
         * indicate that we've handled it here and the Haskell handler
         * doesn't take proper action (e.g., terminate the OS process),
         * the user of the app will be unable to kill/close it. Not
         * good, so disable the delivery for now.
         */
        return false;
    default:
        if (!deliver_event) return true;

#if defined(THREADED_RTS)
        sendIOManagerEvent((StgWord8) ((dwCtrlType<<1) | 1));
#else
        if ( stg_pending_events < N_PENDING_EVENTS ) {
            stg_pending_buf[stg_pending_events] = dwCtrlType;
            stg_pending_events++;
        }

        // we need to wake up awaitEvent()
        abandonRequestWait();
#endif
        return true;
    }
}


/*
 * Function: rts_InstallConsoleEvent()
 *
 * Install/remove a console event handler.
 */
int
rts_InstallConsoleEvent(int action, StgStablePtr *handler)
{
    StgInt previous_hdlr = console_handler;

    switch (action) {
    case STG_SIG_IGN:
        console_handler = STG_SIG_IGN;
        if ( !SetConsoleCtrlHandler(NULL, true) ) {
            errorBelch("warning: unable to ignore console events");
        }
        break;
    case STG_SIG_DFL:
        console_handler = STG_SIG_IGN;
        if ( !SetConsoleCtrlHandler(NULL, false) ) {
            errorBelch("warning: unable to restore default console event "
                       "handling");
        }
        break;
    case STG_SIG_HAN:
#if defined(THREADED_RTS)
        // handler is stored in an MVar in the threaded RTS
        console_handler = STG_SIG_HAN;
#else
        console_handler = (StgInt)*handler;
#endif
        if (previous_hdlr < 0 || previous_hdlr == STG_SIG_HAN) {
          /* Only install generic_handler() once */
          if ( !SetConsoleCtrlHandler(generic_handler, true) ) {
            errorBelch("warning: unable to install console event handler");
          }
        }
        break;
    }

    if (previous_hdlr == STG_SIG_DFL ||
        previous_hdlr == STG_SIG_IGN ||
        previous_hdlr == STG_SIG_HAN) {
        return previous_hdlr;
    } else {
        if (handler != NULL) {
            *handler = (StgStablePtr)previous_hdlr;
        }
        return STG_SIG_HAN;
    }
}

/*
 * Function: rts_HandledConsoleEvent()
 *
 * Signal that a Haskell console event handler has completed its run.
 * The explicit notification that a Haskell handler has completed is
 * required to better handle the delivery of Ctrl-C/Break events whilst
 * an async worker thread is handling a read request on stdin. The
 * Win32 console implementation will abort such a read request when Ctrl-C
 * is delivered. That leaves the worker thread in a bind: should it
 * abandon the request (the Haskell thread reading from stdin has been
 * thrown an exception to signal the delivery of Ctrl-C & hence have
 * aborted the I/O request) or simply ignore the aborted read and retry?
 * (the Haskell thread reading from stdin isn't concerned with the
 * delivery and handling of Ctrl-C.) With both scenarios being
 * possible, the worker thread needs to be told -- that is, did the
 * console event handler cause the IO request to be abandoned?
 *
 */
void
rts_ConsoleHandlerDone (int ev USED_IF_NOT_THREADS)
{
#if !defined(THREADED_RTS)
    if ( (DWORD)ev == CTRL_BREAK_EVENT ||
         (DWORD)ev == CTRL_C_EVENT ) {
        /* only these two cause stdin system calls to abort.. */
        SetEvent(hConsoleEvent); /* event is manual-reset */
        Sleep(0); /* yield */
        ResetEvent(hConsoleEvent); /* turn it back off again */
        // SDM: yeuch, this can't possibly work reliably.
        // I'm not having it in THREADED_RTS.
    }
#endif
}

#if !defined(THREADED_RTS)
/*
 * Function: rts_waitConsoleHandlerCompletion()
 *
 * Esoteric entry point used by worker thread that got woken
 * up as part Ctrl-C delivery.
 */
int
rts_waitConsoleHandlerCompletion()
{
    /* As long as the worker doesn't need to do a multiple wait,
     * let's keep this HANDLE private to this 'module'.
     */
    return (WaitForSingleObject(hConsoleEvent, INFINITE) == WAIT_OBJECT_0);
}
#endif
