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

void longPauseCb(uint64_t dur_ns STG_UNUSED, const char *desc STG_UNUSED) {
  trace(TRACE_gc, "LONG PAUSE(%s) %f", desc, 1.0 * dur_ns / 1e9);
}

void ACQUIRE_LOCK_CHECKED_(Mutex *mutex, int max_msec, const char *desc) {
  struct long_pause_ctx ctx;
  LONG_PAUSE_START(&ctx);
  ACQUIRE_LOCK(mutex);
  LONG_PAUSE_END(&ctx, max_msec, desc);
}

#endif

