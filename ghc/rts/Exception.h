/* -----------------------------------------------------------------------------
 * $Id: Exception.h,v 1.2 2000/03/17 10:24:44 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Exception support
 *
 * ---------------------------------------------------------------------------*/

extern const StgInfoTable blockAsyncExceptionszh_ret_info;
extern const StgInfoTable unblockAsyncExceptionszh_ret_info;

/* Determine whether a thread is interruptible (ie. blocked
 * indefinitely).  Interruptible threads can be sent an exception with
 * killThread# even if they have async exceptions blocked.
 */
static __inline__ int
interruptible(StgTSO *t)
{
  switch (t->why_blocked) {
  case BlockedOnMVar:
  case BlockedOnException:
  case BlockedOnRead:
  case BlockedOnWrite:
  case BlockedOnDelay:
    return 1;
  default:
    return 0;
  }
}
