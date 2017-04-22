/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2005
 *
 * The awaitEvent() interface, for the non-threaded RTS
 *
 * -------------------------------------------------------------------------*/

#pragma once

#if !defined(THREADED_RTS)
/* awaitEvent(bool wait)
 *
 * Checks for blocked threads that need to be woken.
 *
 * Called from STG :  NO
 * Locks assumed   :  sched_mutex
 */
RTS_PRIVATE void awaitEvent(bool wait);  /* In posix/Select.c or
                                          * win32/AwaitEvent.c */
#endif
