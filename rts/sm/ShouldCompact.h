/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2016
 *
 * GC support for immutable non-GCed structures
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 *
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/storage/gc
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#define SHOULDCOMPACT_STATIC 0
#define SHOULDCOMPACT_IN_CNF 1
#define SHOULDCOMPACT_NOTIN_CNF 2
#define SHOULDCOMPACT_PINNED 3

#if !defined(CMINUSMINUS)
extern StgWord shouldCompact (StgCompactNFData *str, StgClosure *p);
#endif
