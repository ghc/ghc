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

#pragma once

/* libraries/ghc-prim/cbits/atomic.c */
StgWord hs_atomic_add8(StgWord x, StgWord val);
StgWord hs_atomic_add16(StgWord x, StgWord val);
StgWord hs_atomic_add32(StgWord x, StgWord val);
StgWord64 hs_atomic_add64(StgWord x, StgWord64 val);
StgWord hs_atomic_sub8(StgWord x, StgWord val);
StgWord hs_atomic_sub16(StgWord x, StgWord val);
StgWord hs_atomic_sub32(StgWord x, StgWord val);
StgWord64 hs_atomic_sub64(StgWord x, StgWord64 val);
StgWord hs_atomic_and8(StgWord x, StgWord val);
StgWord hs_atomic_and16(StgWord x, StgWord val);
StgWord hs_atomic_and32(StgWord x, StgWord val);
StgWord64 hs_atomic_and64(StgWord x, StgWord64 val);
StgWord hs_atomic_nand8(StgWord x, StgWord val);
StgWord hs_atomic_nand16(StgWord x, StgWord val);
StgWord hs_atomic_nand32(StgWord x, StgWord val);
StgWord64 hs_atomic_nand64(StgWord x, StgWord64 val);
StgWord hs_atomic_or8(StgWord x, StgWord val);
StgWord hs_atomic_or16(StgWord x, StgWord val);
StgWord hs_atomic_or32(StgWord x, StgWord val);
StgWord64 hs_atomic_or64(StgWord x, StgWord64 val);
StgWord hs_atomic_xor8(StgWord x, StgWord val);
StgWord hs_atomic_xor16(StgWord x, StgWord val);
StgWord hs_atomic_xor32(StgWord x, StgWord val);
StgWord64 hs_atomic_xor64(StgWord x, StgWord64 val);
StgWord hs_cmpxchg8(StgWord x, StgWord old, StgWord new_);
StgWord hs_cmpxchg16(StgWord x, StgWord old, StgWord new_);
StgWord hs_cmpxchg32(StgWord x, StgWord old, StgWord new_);
StgWord hs_cmpxchg64(StgWord x, StgWord64 old, StgWord64 new_);
StgWord hs_atomicread8(StgWord x);
StgWord hs_atomicread16(StgWord x);
StgWord hs_atomicread32(StgWord x);
StgWord64 hs_atomicread64(StgWord x);
void hs_atomicwrite8(StgWord x, StgWord val);
void hs_atomicwrite16(StgWord x, StgWord val);
void hs_atomicwrite32(StgWord x, StgWord val);
void hs_atomicwrite64(StgWord x, StgWord64 val);

/* libraries/ghc-prim/cbits/bswap.c */
StgWord16 hs_bswap16(StgWord16 x);
StgWord32 hs_bswap32(StgWord32 x);
StgWord64 hs_bswap64(StgWord64 x);

/* TODO: longlong.c */

/* libraries/ghc-prim/cbits/pdep.c */
StgWord64 hs_pdep64(StgWord64 src, StgWord64 mask);
StgWord hs_pdep32(StgWord src, StgWord mask);
StgWord hs_pdep16(StgWord src, StgWord mask);
StgWord hs_pdep8(StgWord src, StgWord mask);

/* libraries/ghc-prim/cbits/pext.c */
StgWord64 hs_pext64(StgWord64 src, StgWord64 mask);
StgWord hs_pext32(StgWord src, StgWord mask);
StgWord hs_pext16(StgWord src, StgWord mask);
StgWord hs_pext8(StgWord src, StgWord mask);

/* libraries/ghc-prim/cbits/popcnt.c */
StgWord hs_popcnt8(StgWord x);
StgWord hs_popcnt16(StgWord x);
StgWord hs_popcnt32(StgWord x);
StgWord hs_popcnt64(StgWord64 x);
StgWord hs_popcnt(StgWord x);

/* libraries/ghc-prim/cbits/word2float.c */
StgFloat hs_word2float32(StgWord x);
StgDouble hs_word2float64(StgWord x);

/* libraries/ghc-prim/cbits/clz.c */
StgWord hs_clz8(StgWord x);
StgWord hs_clz16(StgWord x);
StgWord hs_clz32(StgWord x);
StgWord hs_clz64(StgWord64 x);

/* libraries/ghc-prim/cbits/ctz.c */
StgWord hs_ctz8(StgWord x);
StgWord hs_ctz16(StgWord x);
StgWord hs_ctz32(StgWord x);
StgWord hs_ctz64(StgWord64 x);
