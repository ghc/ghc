/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Exception support
 *
 * ---------------------------------------------------------------------------*/

#ifndef EXCEPTION_H
#define EXCEPTION_H

extern const StgRetInfoTable stg_blockAsyncExceptionszh_ret_info;
extern const StgRetInfoTable stg_unblockAsyncExceptionszh_ret_info;

/* Determine whether a thread is interruptible (ie. blocked
 * indefinitely).  Interruptible threads can be sent an exception with
 * killThread# even if they have async exceptions blocked.
 */
STATIC_INLINE int
interruptible(StgTSO *t)
{
  switch (t->why_blocked) {
  case BlockedOnMVar:
  case BlockedOnException:
  case BlockedOnRead:
  case BlockedOnWrite:
#if defined(mingw32_HOST_OS)
  case BlockedOnDoProc:
#endif
  case BlockedOnDelay:
    return 1;
  // NB. Threaded blocked on foreign calls (BlockedOnCCall) are
  // *not* interruptible.  We can't send these threads an exception.
  default:
    return 0;
  }
}

#endif /* EXCEPTION_H */

