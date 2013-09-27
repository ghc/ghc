/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2006
 *
 * Asynchronous exceptions
 *
 * --------------------------------------------------------------------------*/

#ifndef RAISEASYNC_H
#define RAISEASYNC_H

#define THROWTO_SUCCESS   0
#define THROWTO_BLOCKED   1

#ifndef CMINUSMINUS

#include "BeginPrivate.h"

void throwToSingleThreaded (Capability *cap,
                            StgTSO *tso,
                            StgClosure *exception);

void throwToSingleThreaded_ (Capability *cap,
                             StgTSO *tso,
                             StgClosure *exception,
                             rtsBool stop_at_atomically);

void suspendComputation (Capability *cap,
                         StgTSO *tso,
                         StgUpdateFrame *stop_here);

rtsBool suspendAllComputation (Capability *cap,
                               StgClosure *bh);


MessageThrowTo *throwTo (Capability *cap,      // the Capability we hold
                         StgTSO *source,
                         StgTSO *target,
                         StgClosure *exception); // the exception closure

nat throwToMsg (Capability *cap,
                MessageThrowTo *msg);

int  maybePerformBlockedException (Capability *cap, StgTSO *tso);
void awakenBlockedExceptionQueue  (Capability *cap, StgTSO *tso);

/* Determine whether a thread is interruptible (ie. blocked
 * indefinitely).  Interruptible threads can be sent an exception with
 * killThread# even if they have async exceptions blocked.
 */
INLINE_HEADER int
interruptible(StgTSO *t)
{
  switch (t->why_blocked) {
    case BlockedOnMVar:
    case BlockedOnMVarRead:
    case BlockedOnMsgThrowTo:
    case BlockedOnRead:
    case BlockedOnWrite:
#if defined(mingw32_HOST_OS)
    case BlockedOnDoProc:
#endif
    case BlockedOnDelay:
    case Yielded:
    case BlockedInHaskell:
      return 1;
      // NB. Threaded blocked on foreign calls (BlockedOnCCall) are
      // *not* interruptible.  We can't send these threads an exception.
    default:
      return 0;
  }
}

StgUpdateFrame* findLastUpdateFrame (StgTSO* tso);

#include "EndPrivate.h"

#endif /* CMINUSMINUS */

#endif /* RAISEASYNC_H */

