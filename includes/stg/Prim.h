/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2014-2014
 *
 * Declarations for C fallback primitives implemented by 'ghc-prim' package.
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * -------------------------------------------------------------------------- */

#ifndef PRIM_H
#define PRIM_H

/* libraries/ghc-prim/cbits/bswap.c */
StgWord16 hs_bswap16(StgWord16 x);
StgWord32 hs_bswap32(StgWord32 x);
StgWord64 hs_bswap64(StgWord64 x);

/* TODO: longlong.c */

/* libraries/ghc-prim/cbits/popcnt.c */
StgWord hs_popcnt8(StgWord x);
StgWord hs_popcnt16(StgWord x);
StgWord hs_popcnt32(StgWord x);
StgWord hs_popcnt64(StgWord64 x);
#ifdef i386_HOST_ARCH
StgWord hs_popcnt(StgWord x);
#else
StgWord hs_popcnt(StgWord x);
#endif

/* libraries/ghc-prim/cbits/word2float.c */
StgFloat hs_word2float32(StgWord x);
StgDouble hs_word2float64(StgWord x);

#endif /* PRIM_H */
