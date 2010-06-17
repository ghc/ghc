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

# if defined(PAR)
# define PVM_PE_MASK    0xfffc0000
# define MAX_PVM_PES    MAX_PES
# define MAX_PVM_TIDS   MAX_PES
# define MAX_SLOTS      100000
# endif

/* debugging routines */
void checkSanity        ( rtsBool check_heap );
void checkNurserySanity ( nursery *nursery );
void checkHeap          ( bdescr *bd );
void checkHeapChunk     ( StgPtr start, StgPtr end );
void checkLargeObjects  ( bdescr *bd );
void checkTSO           ( StgTSO* tso );
void checkGlobalTSOList ( rtsBool checkTSOs );
void checkStaticObjects ( StgClosure* static_objects );
void checkStackChunk    ( StgPtr sp, StgPtr stack_end );
StgOffset checkStackFrame ( StgPtr sp );
StgOffset checkClosure  ( StgClosure* p );

void checkMutableList   ( bdescr *bd, nat gen );
void checkMutableLists  ( rtsBool checkTSOs );

void checkRunQueue      (Capability *cap);

void memInventory (rtsBool show);

void checkBQ (StgTSO *bqe, StgClosure *closure);

#include "EndPrivate.h"

#endif /* DEBUG */
 
#endif /* SANITY_H */
