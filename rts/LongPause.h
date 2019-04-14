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

void longPauseCb (uint64_t dur_ns);

#if !defined(CMINUSMINUS)

#if defined(THREADED_RTS)

#include <time.h>

struct long_pause_ctx {
  struct timespec start;
};

INLINE_HEADER void LONG_PAUSE_START(struct long_pause_ctx *ctx) {
  clock_gettime(CLOCK_MONOTONIC, &ctx->start);
}

INLINE_HEADER void LONG_PAUSE_END(struct long_pause_ctx *ctx, int max_msec) {
  struct timespec end;
  clock_gettime(CLOCK_MONOTONIC, &end);
  int64_t dt = (end.tv_sec - ctx->start.tv_sec) * 1000*1000*1000 + end.tv_nsec - ctx->start.tv_nsec;
  if (dt > max_msec * 1000*1000) {
    longPauseCb(dt);
  }
}

void ACQUIRE_LOCK_CHECKED_(Mutex *mutex, int max_msec);

INLINE_HEADER void ACQUIRE_LOCK_CHECKED(Mutex *mutex) {
  ACQUIRE_LOCK_CHECKED_(mutex, 100);
}

#else

#define ACQUIRE_LOCK_CHECKED(l)
#define ACQUIRE_LOCK_CHECKED_(l,m)

#endif /* defined(THREADED_RTS) */

#endif /* !CMINUSMINUS */

#include "EndPrivate.h"

