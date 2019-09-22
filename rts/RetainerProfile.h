/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2001
 * Author: Sungwoo Park
 *
 * Retainer profiling interface.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#if defined(PROFILING)

#include "RetainerSet.h"
#include "TraverseHeap.h"

#include "BeginPrivate.h"

void initRetainerProfiling ( void );
void endRetainerProfiling  ( void );
void retainerProfile       ( void );

// extract the retainer set field from c
#define RSET(c)   ((c)->header.prof.hp.trav.rs)

static inline RetainerSet *
retainerSetOf( const StgClosure *c )
{
    ASSERT( isTravDataValid(c) );
    // StgWord has the same size as pointers, so the following type
    // casting is okay.
    return (RetainerSet *)((StgWord)RSET(c) ^ flip);
}

// Used by GC.c
W_ retainerStackBlocks(void);

#include "EndPrivate.h"

#endif /* PROFILING */
