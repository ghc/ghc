/* -----------------------------------------------------------------------------
 * $Id: Exception.h,v 1.7 2003/11/12 17:49:07 sof Exp $
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
INLINE_HEADER int
interruptible(StgTSO *t)
{
  switch (t->why_blocked) {
  case BlockedOnMVar:
  case BlockedOnException:
  case BlockedOnRead:
  case BlockedOnWrite:
#if defined(mingw32_TARGET_OS)
  case BlockedOnDoProc:
#endif
  case BlockedOnDelay:
    return 1;
  default:
    return 0;
  }
}
