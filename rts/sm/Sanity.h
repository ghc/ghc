/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-1999
 *
 * Prototypes for functions in Sanity.c
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#if defined(DEBUG)

#include "BeginPrivate.h"

# if defined(PAR)
# define PVM_PE_MASK    0xfffc0000
# define MAX_PVM_PES    MAX_PES
# define MAX_PVM_TIDS   MAX_PES
# define MAX_SLOTS      100000
# endif

/* debugging routines */
void checkSanity        ( bool after_gc, bool major_gc );
void checkNurserySanity ( nursery *nursery );
void checkHeapChain     ( bdescr *bd );
void checkHeapChunk     ( StgPtr start, StgPtr end );
void checkLargeObjects  ( bdescr *bd );
void checkTSO           ( StgTSO* tso );
void checkGlobalTSOList ( bool checkTSOs );
void checkStaticObjects ( StgClosure* static_objects );
void checkStackChunk    ( StgPtr sp, StgPtr stack_end );
StgOffset checkStackFrame ( StgPtr sp );
StgOffset checkClosure  ( const StgClosure* p );
void checkNonmovingHeap ( const struct NonmovingHeap *heap );

void checkRunQueue      (Capability *cap);

void memInventory (bool show);

void checkBQ (StgTSO *bqe, StgClosure *closure);

#include "EndPrivate.h"

#endif /* DEBUG */
