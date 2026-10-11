/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2005
 *
 * Interface to the OS-specific implementation of a regular time signal.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"

typedef struct Ticker Ticker; // opaque

typedef void (*TickProc)(int);

/* The ticker is initialised in a paused state. Use unpauseTicker to start. */
Ticker *initTicker(Time interval, TickProc handle_tick);

/* Stop the ticker prior to releasing resources with freeTicker().
 * The stopTicker action is *synchronous*. When it returns the caller is
 * guaranteed that the tick action will not be called again.
 */
void stopTicker(Ticker *ticker);

/* Release the ticker's resources. The ticker must be stopped first using
 * stopTicker(), or alternatively, in the case of posix it should be called in
 * the child process after a fork().
 */
void freeTicker(Ticker *ticker);

/* Pause and unpause (resume) the ticker.
 *
 * The pauseTicker and unpauseTicker actions are *asynchronous*. After calling
 * pauseTicker, the ticker will pause eventually, but there may be another tick
 * action before it does pause (and theoretically there could be several but
 * in practice this is unlikely). Similarly, after calling unpauseTicker the
 * ticker will start up again eventually, but there is an unspecified delay
 * between the unpause and the next tick action (but in practice it is short).
 *
 * This should be used for the purpose of *efficiency*: to avoid unnecessary
 * OS thread wakeups caused by the ticker.
 *
 * These should *not* be used for the purpose of *concurrency safety*: to
 * prevent the tick action from running concurrently with some other critical
 * section. The synchronous case is not provided because it is not currently
 * needed (and proper locking is often a better solution anyway).
 *
 * The pairing of unpauseTicker and the handle_tick action form a
 * synchonises-with relation: values written before unpauseTicker can be
 * read from the resulting handle_tick action.
 *
 * It *is* safe to call these functions from within the tick handler itself.
 *
 * It is safe to use these functions concurrently from multiple threads, but
 * note that they *are* idempotent. This means it is not appropriate to use
 * paired pause/unpause calls concurrently. They can be used by threads based
 * on consistent use of some shared state or observation.
 */
void pauseTicker(Ticker *ticker);
void unpauseTicker(Ticker *ticker);

#include "EndPrivate.h"
