/* -----------------------------------------------------------------------------
 * $Id: Exception.h,v 1.5 2002/12/11 15:36:42 simonmar Exp $
 *
 * (c) The GHC Team, 1998-2000
 *
 * Exception support
 *
 * ---------------------------------------------------------------------------*/

extern const StgRetInfoTable stg_blockAsyncExceptionszh_ret_info;
extern const StgRetInfoTable stg_unblockAsyncExceptionszh_ret_info;

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
