/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2006
 *
 * Asynchronous exceptions
 *
 * --------------------------------------------------------------------------*/

#pragma once

#define THROWTO_SUCCESS   0
#define THROWTO_BLOCKED   1

#if !defined(CMINUSMINUS)

#include "BeginPrivate.h"

StgTSO* raiseAsync (Capability *cap,
                    StgTSO *tso,
                    StgClosure *exception,
                    bool stop_at_atomically,
                    StgUpdateFrame *stop_here);

void throwToSingleThreaded (Capability *cap,
                            StgTSO *tso,
                            StgClosure *exception);

void throwToSingleThreaded_ (Capability *cap,
                             StgTSO *tso,
                             StgClosure *exception,
                             bool stop_at_atomically);

void throwToSelf (Capability *cap,
                  StgTSO *tso,
                  StgClosure *exception);

void suspendComputation (Capability *cap,
                         StgTSO *tso,
                         StgUpdateFrame *stop_here);

MessageThrowTo *throwTo (Capability *cap,      // the Capability we hold
                         StgTSO *source,
                         StgTSO *target,
                         StgClosure *exception); // the exception closure

uint32_t throwToMsg (Capability *cap,
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
  case BlockedOnSTM:
  case BlockedOnMVarRead:
  case BlockedOnMsgThrowTo:
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

#include "EndPrivate.h"

#endif /* CMINUSMINUS */
