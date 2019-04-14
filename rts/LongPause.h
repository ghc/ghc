/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2001-2005
 *
 * Accessing OS threads functionality in a (mostly) OS-independent
 * manner.
 *
 * --------------------------------------------------------------------------*/

#pragma once

#include "rts/OSThreads.h"

#include "BeginPrivate.h"

void longPauseCb (uint64_t dur_ns, const char *desc);

#if !defined(CMINUSMINUS)

#if defined(THREADED_RTS)

#include <time.h>

struct long_pause_ctx {
  struct timespec start;
};

INLINE_HEADER void LONG_PAUSE_START(struct long_pause_ctx *ctx) {
  clock_gettime(CLOCK_MONOTONIC, &ctx->start);
}

INLINE_HEADER void LONG_PAUSE_END(struct long_pause_ctx *ctx, int max_msec, const char *desc) {
  struct timespec end;
  clock_gettime(CLOCK_MONOTONIC, &end);
  int64_t dt = (end.tv_sec - ctx->start.tv_sec) * 1000*1000*1000 + end.tv_nsec - ctx->start.tv_nsec;
  if (dt > max_msec * 1000*1000) {
    longPauseCb(dt, desc);
  }
}

/* Acquire the given lock, checking that it takes no more than max_msecs to do
 * so.
 */
void ACQUIRE_LOCK_CHECKED_(Mutex *mutex, int max_msec, const char *desc);

/* Acquire the given lock, checking that it takes a reasonable amount of time
 * to do so.
 */
INLINE_HEADER void ACQUIRE_LOCK_CHECKED(Mutex *mutex, const char *desc) {
  ACQUIRE_LOCK_CHECKED_(mutex, 100, desc);
}

#else

struct long_pause_ctx {};
INLINE_HEADER void LONG_PAUSE_START(struct long_pause_ctx *ctx STG_UNUSED) {}
INLINE_HEADER void LONG_PAUSE_END(struct long_pause_ctx *ctx STG_UNUSED, int max_msec STG_UNUSED, const char *desc STG_UNUSED) {}

#define ACQUIRE_LOCK_CHECKED(l,desc)
#define ACQUIRE_LOCK_CHECKED_(l,m,desc)

#endif /* defined(THREADED_RTS) */

#endif /* !CMINUSMINUS */

#include "EndPrivate.h"

