/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2005
 *
 * The awaitEvent() interface, for the non-threaded RTS
 *
 * -------------------------------------------------------------------------*/

#ifndef AWAITEVENT_H
#define AWAITEVENT_H

#if !defined(THREADED_RTS)
/* awaitEvent(rtsBool wait)
 *
 * Checks for blocked threads that need to be woken.
 *
 * Called from STG :  NO
 * Locks assumed   :  sched_mutex
 */
void awaitEvent(rtsBool wait);  /* In posix/Select.c or
				 * win32/AwaitEvent.c */
#endif

#endif /* SELECT_H */
