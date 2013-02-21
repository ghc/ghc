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

#include "BeginPrivate.h"

void initRetainerProfiling ( void );
void endRetainerProfiling  ( void );
void retainerProfile       ( void );
void resetStaticObjectForRetainerProfiling( StgClosure *static_objects );

// flip is either 1 or 0, changed at the beginning of retainerProfile()
// It is used to tell whether a retainer set has been touched so far
// during this pass.
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
extern W_ retainerStackBlocks ( void );
#endif

#include "EndPrivate.h"

#endif /* PROFILING */

#endif /* RETAINERPROFILE_H */
