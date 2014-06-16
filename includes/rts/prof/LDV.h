/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow, 2009
 *
 * Lag/Drag/Void profiling.
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTS_PROF_LDV_H
#define RTS_PROF_LDV_H

#ifdef PROFILING

/* retrieves the LDV word from closure c */
#define LDVW(c)                 (((StgClosure *)(c))->header.prof.hp.ldvw)

/*
 * Stores the creation time for closure c. 
 * This macro is called at the very moment of closure creation.
 *
 * NOTE: this initializes LDVW(c) to zero, which ensures that there
 * is no conflict between retainer profiling and LDV profiling,
 * because retainer profiling also expects LDVW(c) to be initialised
 * to zero.
 */

#ifdef CMINUSMINUS

#else

#define LDV_RECORD_CREATE(c)   \
  LDVW((c)) = ((StgWord)RTS_DEREF(era) << LDV_SHIFT) | LDV_STATE_CREATE

#endif

#else  /* !PROFILING */

#define LDV_RECORD_CREATE(c)   /* nothing */

#endif /* PROFILING */

#endif /* STGLDVPROF_H */
