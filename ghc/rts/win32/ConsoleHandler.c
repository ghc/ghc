/*
 * Console control handler support.
 *
 */
#include "Rts.h"
#include <windows.h>
#include "ConsoleHandler.h"
#include "SchedAPI.h"
#include "Schedule.h"
#include "RtsUtils.h"
#include "RtsFlags.h"
#include "StablePriv.h"

extern int stg_InstallConsoleEvent(int action, StgStablePtr *handler);

static BOOL WINAPI shutdown_handler(DWORD dwCtrlType);
static BOOL WINAPI generic_handler(DWORD dwCtrlType);

static rtsBool deliver_event = rtsTrue;
static StgInt console_handler = STG_SIG_DFL;

#define N_PENDING_EVENTS 16
StgInt stg_pending_events = 0;           /* number of undelivered events */
DWORD stg_pending_buf[N_PENDING_EVENTS]; /* their associated event numbers. */

/*
 * Function: initUserSignals()
 *
 * Initialize the console handling substrate.
 */
void
initUserSignals(void)
{
    stg_pending_events = 0;
    console_handler = STG_SIG_DFL;
    return;
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
    
    case CTRL_C_EVENT:
    case CTRL_BREAK_EVENT:
    case CTRL_CLOSE_EVENT:

	// If we're already trying to interrupt the RTS, terminate with
	// extreme prejudice.  So the first ^C tries to exit the program
	// cleanly, and the second one just kills it.
	if (interrupted) {
	    stg_exit(EXIT_INTERRUPTED);
	} else {
	    interruptStgRts();
	}
	return TRUE;

	/* shutdown + logoff events are not handled here. */
    default:
	return FALSE;
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
    if ( !SetConsoleCtrlHandler(shutdown_handler, TRUE) ) {
	prog_belch("warning: failed to install default console handler");
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
    deliver_event = rtsFalse;
}


/*
 * Function: unblockUserSignals()
 *
 * The inverse of blockUserSignals(); re-enable the deliver of console events.
 */
void
unblockUserSignals(void)
{
    deliver_event = rtsTrue;
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


/*
 * Function: startSignalHandlers()
 *
 * Run the handlers associated with the stacked up console events. Console
 * event delivery is blocked for the duration of this call.
 */
void startSignalHandlers(void)
{
    StgStablePtr handler;

    if (console_handler < 0) {
	return;
    }
    blockUserSignals();
    
    handler = deRefStablePtr((StgStablePtr)console_handler);
    while (stg_pending_events > 0) {
	stg_pending_events--;
	scheduleThread(
	    createIOThread(RtsFlags.GcFlags.initialStkSize, 
			   rts_apply((StgClosure *)handler,
				     rts_mkInt(stg_pending_buf[stg_pending_events]))));
    }
    unblockUserSignals();
}


/*
 * Function: markSignalHandlers()
 *
 * Evacuate the handler stack. _Assumes_ that console event delivery
 * has already been blocked.
 */
void markSignalHandlers (evac_fn evac)
{
    if (console_handler >= 0) {
	StgPtr p = deRefStablePtr((StgStablePtr)console_handler);
	evac((StgClosure**)&p);
    }
}


/*
 * Function: handleSignalsInThisThread()
 * 
 * Have current (OS) thread assume responsibility of handling console events/signals.
 * Currently not used (by the console event handling code.)
 */
void handleSignalsInThisThread(void)
{
    return;
}

/* 
 * Function: generic_handler()
 *
 * Local function which handles incoming console event (done in a sep OS thread),
 * recording the event in stg_pending_events. 
 */
static BOOL WINAPI generic_handler(DWORD dwCtrlType)
{
    /* Ultra-simple -- up the counter + signal a switch. */
    if ( stg_pending_events < N_PENDING_EVENTS ) {
	stg_pending_buf[stg_pending_events] = dwCtrlType;
	stg_pending_events++;
    }
    context_switch = 1;
    return TRUE;
}


/*
 * Function: stg_InstallConsoleEvent()
 *
 * Install/remove a console event handler.
 */
int
stg_InstallConsoleEvent(int action, StgStablePtr *handler)
{
    StgInt previous_hdlr = console_handler;

    switch (action) {
    case STG_SIG_IGN:
	console_handler = STG_SIG_IGN;
	if ( !SetConsoleCtrlHandler(NULL, TRUE) ) {
	    prog_belch("warning: unable to ignore console events");
	}
	break;
    case STG_SIG_DFL:
	console_handler = STG_SIG_IGN;
	if ( !SetConsoleCtrlHandler(NULL, FALSE) ) {
	    prog_belch("warning: unable to restore default console event handling");
	}
	break;
    case STG_SIG_HAN:
	console_handler = (StgInt)*handler;
	if ( previous_hdlr < 0 ) {
	  /* Only install generic_handler() once */
	  if ( !SetConsoleCtrlHandler(generic_handler, TRUE) ) {
	    prog_belch("warning: unable to install console event handler");
	  }
	}
	break;
    }
    
    if (previous_hdlr == STG_SIG_DFL || 
	previous_hdlr == STG_SIG_IGN) {
	return previous_hdlr;
    } else {
	*handler = (StgStablePtr)previous_hdlr;
	return STG_SIG_HAN;
    }
}
