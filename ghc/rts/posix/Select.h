/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2005
 *
 * Prototypes for functions in Select.c
 *
 * -------------------------------------------------------------------------*/

#ifndef SELECT_H
#define SELECT_H

#if !defined(THREADED_RTS)
/* In Select.c */
extern lnat RTS_VAR(timestamp);

/* awaitEvent(rtsBool wait)
 *
 * Checks for blocked threads that need to be woken.
 *
 * Called from STG :  NO
 * Locks assumed   :  sched_mutex
 */
void awaitEvent(rtsBool wait);  /* In Select.c */
#endif

#endif /* SELECT_H */
