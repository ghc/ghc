/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow, 2004
 *
 * Lag/Drag/Void profiling.
 *
 * ---------------------------------------------------------------------------*/

#ifndef STGLDVPROF_H
#define STGLDVPROF_H

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
#ifndef CMINUSMINUS
#define LDV_RECORD_CREATE(c)   \
  LDVW((c)) = ((StgWord)RTS_DEREF(era) << LDV_SHIFT) | LDV_STATE_CREATE
#endif

#ifdef CMINUSMINUS
#define LDV_RECORD_DEAD_FILL_SLOP_DYNAMIC(c) \
  foreign "C" LDV_recordDead_FILL_SLOP_DYNAMIC(c "ptr")
#else
#define LDV_RECORD_DEAD_FILL_SLOP_DYNAMIC(c) \
  LDV_recordDead_FILL_SLOP_DYNAMIC(c)
#endif

#else  /* !PROFILING */

#define LDV_RECORD_CREATE(c)   /* nothing */
#define LDV_RECORD_DEAD_FILL_SLOP_DYNAMIC(c)  /* nothing */

#endif /* PROFILING */
#endif /* STGLDVPROF_H */
