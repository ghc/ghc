/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Signal processing / handling. This is the shared API to the subsystems for
 * POSIX signals and Win32 console events.
 *
 * Platform specific APIs live in win32/ConsoleHandler.h and posix/Signals.h
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

/* Finalise signal and console events handling.
 */
void finiUserSignals(void);

#include "EndPrivate.h"

#endif /* RTS_USER_SIGNALS */
