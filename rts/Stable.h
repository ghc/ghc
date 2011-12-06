/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * Stable Pointers: A stable pointer is represented as an index into
 * the stable pointer table.
 *
 * StgStablePtr used to be a synonym for StgWord, but stable pointers
 * are guaranteed to be void* on the C-side, so we have to do some
 * occasional casting. Size is not a matter, because StgWord is always
 * the same size as a void*.
 *
 * ---------------------------------------------------------------------------*/

#ifndef STABLE_H
#define STABLE_H

#include "sm/GC.h" // for evac_fn below

#include "BeginPrivate.h"

void    freeStablePtr         ( StgStablePtr sp );

void    initStablePtrTable    ( void );
void    exitStablePtrTable    ( void );
StgWord lookupStableName      ( StgPtr p );

void    markStablePtrTable    ( evac_fn evac, void *user );
void    threadStablePtrTable  ( evac_fn evac, void *user );
void    gcStablePtrTable      ( void );
void    updateStablePtrTable  ( rtsBool full );

void    stablePtrPreGC        ( void );
void    stablePtrPostGC       ( void );

#ifdef THREADED_RTS
// needed by Schedule.c:forkProcess()
extern Mutex stable_mutex;
#endif

#include "EndPrivate.h"

#endif /* STABLE_H */
