/* -----------------------------------------------------------------------------
 * $Id: Exception.h,v 1.3 2000/11/13 14:40:37 simonmar Exp $
 *
 * (c) The GHC Team, 1998-2000
 *
 * Exception support
 *
 * ---------------------------------------------------------------------------*/

extern const StgInfoTable stg_blockAsyncExceptionszh_ret_info;
extern const StgInfoTable stg_unblockAsyncExceptionszh_ret_info;

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
