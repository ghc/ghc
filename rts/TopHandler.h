/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2016
 *
 * Top-level handler support
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include <BeginPrivate.h>

#include <rts/Types.h>
#include <rts/storage/Closures.h>
#include <stg/Types.h>

// Initialize the top handler subsystem
void initTopHandler(void);

// Exit the top handler subsystem
void exitTopHandler(void);

// Get the thread that handles ctrl-c, etc
// Returns NULL if there is no such thread
StgTSO *getTopHandlerThread(void);

#include <EndPrivate.h>

// Called from Haskell
void rts_setMainThread(StgWeak *ptr);
