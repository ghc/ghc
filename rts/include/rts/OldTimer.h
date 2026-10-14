/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1995-2009
 *
 * Compatibility interface to what historically was the RTS timer signal.
 * None of these functions do anything anymore. Do not use them.
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * ---------------------------------------------------------------------------*/

#pragma once

/* Deprecated exported functions. Now no-ops.
 * Historically they were used by the process and unix libraries to disable
 * the signal-based interval timer, since otherwise the timer signal would
 * keep going off in the child process and confusing everything. The interval
 * timer no longer uses signals, so there is no need any more for libraries to
 * disable the timer. Also, the timer internal API has changed.
 *
 * They are however stealing the good names! We're forced to use run/halt
 * until we can get rid of these entirely.
 */
void startTimer (void)
  __attribute__((deprecated("startTimer is now a no-op. See issue #27073")));

void stopTimer (void)
  __attribute__((deprecated("startTimer is now a no-op. See issue #27073")));

#if !defined(mingw32_HOST_OS) && !defined(wasm32_HOST_ARCH)
/* This is obsolete, but is used in the unix package for now.
 *
 * It used to report the signal that the RTS used for its timer. The RTS no
 * longer uses signals, so this information is redundant.
 */
int rtsTimerSignal (void)
  __attribute__((deprecated("rtsTimerSignal is now meaningless. "
                            "See issue #27073")));
#endif
