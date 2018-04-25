/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Signal processing / handling.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#if !defined(mingw32_HOST_OS)

#include "posix/Signals.h"

#elif defined(mingw32_HOST_OS)

#include "win32/ConsoleHandler.h"

#else

#define signals_pending() (false)

#endif

#if RTS_USER_SIGNALS

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

/*
 * Function: awaitUserSignals()
 *
 * Wait for the next console event. Currently a NOP (returns immediately.)
 */
void awaitUserSignals(void);

/*
 * Function: markSignalHandlers()
 *
 * Evacuate the handler queue. _Assumes_ that console event delivery
 * has already been blocked.
 */
void markSignalHandlers (evac_fn evac, void *user);

#include "EndPrivate.h"

#endif /* RTS_USER_SIGNALS */
