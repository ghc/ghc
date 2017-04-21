/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2016
 *
 * GC support for immutable non-GCed structures
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 *
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/GC
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
