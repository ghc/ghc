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

void startTimer (void);    // Deprecated: see issue #27073
void stopTimer (void);     // Deprecated: see issue #27073
int rtsTimerSignal (void); // Deprecated: see issue #27073
