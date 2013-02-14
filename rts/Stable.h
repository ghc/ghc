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

/* Use the "Unsafe" one after manually locking with stableLock/stableUnlock */
void    freeStablePtrUnsafe   ( StgStablePtr sp );

void    initStableTables      ( void );
void    exitStableTables      ( void );
StgWord lookupStableName      ( StgPtr p );

/* Call given function on every stable ptr. markStableTables depends
 * on the function updating its pointers in case the object is
 * moved. */
/* TODO: This also remembers old stable name addresses, which isn't
 * necessary in some contexts markStableTables is called from.
 * Consider splitting it.
 */
void    markStableTables      ( evac_fn evac, void *user );

void    threadStableTables    ( evac_fn evac, void *user );
void    gcStableTables        ( void );
void    updateStableTables    ( rtsBool full );

void    stableLock            ( void );
void    stableUnlock          ( void );

#ifdef THREADED_RTS
// needed by Schedule.c:forkProcess()
extern Mutex stable_mutex;
#endif

#include "EndPrivate.h"

#endif /* STABLE_H */
