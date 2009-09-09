/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2001
 * Author: Sungwoo Park
 *
 * Retainer profiling interface.
 *
 * ---------------------------------------------------------------------------*/

#ifndef RETAINERPROFILE_H
#define RETAINERPROFILE_H

#ifdef PROFILING

#include "RetainerSet.h"

BEGIN_RTS_PRIVATE

void initRetainerProfiling ( void );
void endRetainerProfiling  ( void );
void retainerProfile       ( void );
void resetStaticObjectForRetainerProfiling( StgClosure *static_objects );

extern StgWord flip;

// extract the retainer set field from c
#define RSET(c)   ((c)->header.prof.hp.rs)

#define isRetainerSetFieldValid(c) \
  ((((StgWord)(c)->header.prof.hp.rs & 1) ^ flip) == 0)

static inline RetainerSet *
retainerSetOf( StgClosure *c )
{
    ASSERT( isRetainerSetFieldValid(c) );
    // StgWord has the same size as pointers, so the following type
    // casting is okay.
    return (RetainerSet *)((StgWord)RSET(c) ^ flip);
}

// Used by Storage.c:memInventory()
#ifdef DEBUG
extern lnat retainerStackBlocks ( void );
#endif

END_RTS_PRIVATE

#endif /* PROFILING */

#endif /* RETAINERPROFILE_H */
