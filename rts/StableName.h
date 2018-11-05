/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "sm/GC.h" // for evac_fn below

#include "BeginPrivate.h"

void    initStableNameTable   ( void );
void    exitStableNameTable      ( void );
StgWord lookupStableName      ( StgPtr p );

void    rememberOldStableNameAddresses ( void );

void    threadStableNameTable ( evac_fn evac, void *user );
void    gcStableNameTable     ( void );
void    updateStableNameTable ( bool full );

void    stableNameLock            ( void );
void    stableNameUnlock          ( void );

#if defined(THREADED_RTS)
// needed by Schedule.c:forkProcess()
extern Mutex stable_name_mutex;
#endif

#include "EndPrivate.h"
