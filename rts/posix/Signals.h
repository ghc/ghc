/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Extra posix-specific signal processing / handling functions. This is in
 * addition the shared API in RtsSignals.h
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"

#if !defined(THREADED_RTS)
/*
 * Function: startPendingSignalHandlers()
 *
 * If there are any queued up win32 console events, run the handlers
 * associated with them.
 */
void startPendingSignalHandlers(Capability *cap);
#endif

#include "EndPrivate.h"

