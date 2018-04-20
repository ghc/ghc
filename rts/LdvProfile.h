/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2001
 * Author: Sungwoo Park
 *
 * Lag/Drag/Void profiling.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#if defined(PROFILING)

#include "ProfHeap.h"

RTS_PRIVATE void LdvCensusForDead ( uint32_t );
RTS_PRIVATE void LdvCensusKillAll ( void );

// Creates a 0-filled slop of size 'howManyBackwards' backwards from the
// address 'from'.
//
// Invoked when:
//   1) Hp is incremented and exceeds HpLim (in Updates.cmm).
//   2) copypart() is called (in GC.c).
#define LDV_FILL_SLOP(from, howMany)    \
  if (era > 0) {                                \
    int i;                                      \
    for (i = 0;i < (howMany); i++)      \
      ((StgWord *)(from))[i] = 0;               \
  }

// Informs the LDV profiler that closure c has just been evacuated.
// Evacuated objects are no longer needed, so we just store its original size in
// the LDV field.
#define SET_EVACUAEE_FOR_LDV(c, size)   \
    LDVW((c)) = (size)

#endif /* PROFILING */
