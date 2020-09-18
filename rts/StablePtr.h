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

#pragma once

#include "sm/GC.h" // for evac_fn below

#include "BeginPrivate.h"

void    freeStablePtr         ( StgStablePtr sp );

/* Use the "Unsafe" one after only when manually locking and
   unlocking with stablePtrLock/stablePtrUnlock */
void    freeStablePtrUnsafe   ( StgStablePtr sp );

void    initStablePtrTable      ( void );
void    exitStablePtrTable      ( void );

/* Call given function on every stable ptr. markStablePtrTable depends
 * on the function updating its pointers in case the object is
 * moved.
 */
void    markStablePtrTable    ( evac_fn evac, void *user );

void    threadStablePtrTable  ( evac_fn evac, void *user );

void    stablePtrLock         ( void );
void    stablePtrUnlock       ( void );

#if defined(THREADED_RTS)
// needed by Schedule.c:forkProcess()
extern Mutex stable_ptr_mutex;
#endif

#include "EndPrivate.h"
