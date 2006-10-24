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
void throwToSingleThreaded (Capability *cap,
			    StgTSO *tso,
			    StgClosure *exception);

void throwToSingleThreaded_ (Capability *cap, 
			     StgTSO *tso, 
			     StgClosure *exception, 
			     rtsBool stop_at_atomically,
			     StgPtr stop_here);

void suspendComputation (Capability *cap, 
			 StgTSO *tso, 
			 StgPtr stop_here);

nat throwTo (Capability *cap,	         // the Capability we hold 
	     StgTSO *source,	         // the TSO sending the exception
	     StgTSO *target,             // the TSO receiving the exception
	     StgClosure *exception,      // the exception closure
	     /*[out]*/ void **out   // pass to throwToReleaseTarget()
    );

#ifdef THREADED_RTS
void throwToReleaseTarget (void *tso);
#endif

void maybePerformBlockedException (Capability *cap, StgTSO *tso);
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

#endif /* CMINUSMINUS */

#endif /* RAISEASYNC_H */

