/* -----------------------------------------------------------------------------
 * $Id: LdvProfile.h,v 1.1 2001/11/22 14:25:12 simonmar Exp $
 *
 * (c) The GHC Team, 2001
 * Author: Sungwoo Park
 *
 * Lag/Drag/Void profiling.
 *
 * ---------------------------------------------------------------------------*/

#ifndef LDVPROFILE_H
#define LDVPROFILE_H

#ifdef PROFILING

#include "ProfHeap.h"

void  LDV_recordDead_FILL_SLOP_DYNAMIC( StgClosure *p );

// Precesses a closure 'c' being destroyed whose size is 'size'.
// Make sure that LDV_recordDead() is not invoked on 'inherently used' closures
// such as TSO; they should not be involved in computing dragNew or voidNew.
// 
// Note: ldvTime is 0 if LDV profiling is turned off.
//       ldvTime is > 0 if LDV profiling is turned on.
//       size does not include StgProfHeader.
//
// Even though ldvTime is checked in both LdvCensusForDead() and 
// LdvCensusKillAll(), we still need to make sure that ldvTime is > 0 because 
// LDV_recordDead() may be called from elsewhere in the runtime system. E.g., 
// when a thunk is replaced by an indirection object.

static inline void
LDV_recordDead( StgClosure *c, nat size )
{
    if (ldvTime > 0 && closureSatisfiesConstraints(c)) {
	nat t;
	size -= sizeofW(StgProfHeader);
	if ((LDVW((c)) & LDV_STATE_MASK) == LDV_STATE_CREATE) {
	    t = (LDVW((c)) & LDV_CREATE_MASK) >> LDV_SHIFT;
	    if (t < ldvTime) {
		gi[t].voidNew += (int)size;
		gi[ldvTime].voidNew -= (int)size;
	    }
	} else {
	    t = LDVW((c)) & LDV_LAST_MASK;
	    if (t + 1 < ldvTime) {
		gi[t + 1].dragNew += size;
		gi[ldvTime].dragNew -= size;
	    }
	}
    }
}

extern void initLdvProfiling ( void );
extern void endLdvProfiling  ( void );
extern void LdvCensus        ( void );
extern void LdvCensusForDead ( nat );
extern void LdvCensusKillAll ( void );

#endif /* PROFILING */

#endif /* LDVPROFILE_H */
