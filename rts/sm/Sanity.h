/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-1999
 *
 * Prototypes for functions in Sanity.c
 *
 * ---------------------------------------------------------------------------*/

#ifndef SANITY_H
#define SANITY_H

#ifdef DEBUG

#include "BeginPrivate.h"

/* debugging routines */
void checkSanity (rtsBool local_only, rtsBool after_gc, rtsBool major_gc,
                  nat cap_no);
void checkNurserySanity (nat cap_no);

void checkHeapChain       (bdescr *bd);
void checkGlobalHeapChain (bdescr *bd);
void checkPrimHeapChain   (bdescr *bd);

void checkHeapChunk     ( StgPtr start, StgPtr end );
void checkLargeObjects  ( bdescr *bd );
void checkTSO           ( StgTSO* tso );
void checkGlobalTSOList ( rtsBool checkTSOs );
void checkStaticObjects ( StgClosure* static_objects );
void checkStackChunk    ( StgPtr sp, StgPtr stack_end );
StgOffset checkStackFrame    (StgPtr sp);

StgOffset checkClosure       (StgClosure* p);
StgOffset checkGlobalClosure (StgClosure* p);

void checkRunQueue      (Capability *cap);

void memInventory (rtsBool show);

void checkBQ (StgTSO *bqe, StgClosure *closure);

/* tools for use in gdb */

void findPtr         (StgPtr p, int follow);
void findPtrAnywhere (StgPtr p);

void findBlock       (bdescr *bd);
void findBlockInList (bdescr *bd, bdescr *list);

#include "EndPrivate.h"

#endif /* DEBUG */
 
#endif /* SANITY_H */
