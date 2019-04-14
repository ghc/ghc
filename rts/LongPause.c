/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2001-2005
 *
 * Catching long lock-acquisition pauses.
 *
 * --------------------------------------------------------------------------*/


#include "PosixSource.h"

#include "Rts.h"
#include "Trace.h"
#include "LongPause.h"

#if defined(THREADED_RTS)

void longPauseCb(uint64_t dur_ns STG_UNUSED) {
  trace(1, "LONG PAUSE %f", 1.0 * dur_ns / 1e9);
  debugBelch("LONG PAUSE %f", 1.0 * dur_ns / 1e9);
}

void ACQUIRE_LOCK_CHECKED_(Mutex *mutex, int max_msec) {
  struct long_pause_ctx ctx;
  LONG_PAUSE_START(&ctx);
  ACQUIRE_LOCK(mutex);
  LONG_PAUSE_END(&ctx, max_msec);
}

#endif

