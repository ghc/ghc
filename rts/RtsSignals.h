/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Signal processing / handling. This is the shared API to the subsystems for
 * POSIX signals and Win32 console events.
 *
 * Platform specific APIs live in posix/Signals.h and win32/ConsoleHandler.h
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#if defined(RTS_USER_SIGNALS)

#include "BeginPrivate.h"

/*
 * Function: initUserSignals()
 *
 * Initialize the console handling substrate.
 */
void initUserSignals(void);

/*
 * Function: initDefaultHandlers()
 *
 * Install any default signal/console handlers. Currently we install a
 * Ctrl+C handler that shuts down the RTS in an orderly manner.
 */
void initDefaultHandlers(void);
void resetDefaultHandlers(void);

void freeSignalHandlers(void);

/* Tear down and shut down user signal processing.
 * This is called *after* freeSignalHandlers, but unconditionally!
 * TODO: unify this and freeSignalHandlers together, and make them make sense!
 */
void finiUserSignals(void);

/*
 * Function: startPendingSignalHandlers()
 *
 * If there are any queued up posix signals or win32 console events, run the
 * handlers associated with them. This is used by some in-RTS I/O managers.
 */
#if !defined(THREADED_RTS)
void startPendingSignalHandlers(Capability *cap);
#endif

#include "EndPrivate.h"

#endif /* RTS_USER_SIGNALS */
