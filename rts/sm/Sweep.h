/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2008
 *
 * Simple mark/sweep, collecting whole blocks.
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 * 
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/storage/gc
 *
 * ---------------------------------------------------------------------------*/

#pragma once

RTS_PRIVATE void sweep(generation *gen);
