/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005, 2026
 *
 * Shared utilities for I/O managers: signal handling functionality.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"

/*
 * Function: initUserSignals()
 *
 * Initialize the console handling substrate.
 */
void initUserSignals(void);

/* Finalise signal and console events handling.
 */
void finiUserSignals(void);

#if !defined(THREADED_RTS)
/*
 * Function: startPendingSignalHandlers()
 *
 * If there are any queued up posix signals, run the handlers
 * associated with them.
 */
void startPendingSignalHandlers(Capability *cap);
#endif

int installUserSignalHandler(int sig, int spi, void *mask, bool nocldstop);

#include "EndPrivate.h"

