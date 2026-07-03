/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1995-2009
 *
 * Interface to the RTS timer signal (uses OS-dependent Ticker.h underneath)
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * ---------------------------------------------------------------------------*/

#pragma once

RTS_PUBLIC void startTimer (void);
RTS_PUBLIC void stopTimer  (void);
RTS_PUBLIC int rtsTimerSignal (void); // Deprecated: see issue #27073
