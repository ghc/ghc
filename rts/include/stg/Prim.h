/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2014-2014
 *
 * Declarations for C fallback primitives implemented by 'ghc-internal' package.
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * -------------------------------------------------------------------------- */

#pragma once

/* libraries/ghc-internal/cbits/atomic.c */
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
StgWord64 hs_cmpxchg64(StgWord x, StgWord64 old, StgWord64 new_);
StgWord hs_atomicread8(StgWord x);
StgWord hs_atomicread16(StgWord x);
StgWord hs_atomicread32(StgWord x);
StgWord64 hs_atomicread64(StgWord x);
void hs_atomicwrite8(StgWord x, StgWord val);
void hs_atomicwrite16(StgWord x, StgWord val);
void hs_atomicwrite32(StgWord x, StgWord val);
void hs_atomicwrite64(StgWord x, StgWord64 val);
StgWord hs_xchg8(StgWord x, StgWord val);
StgWord hs_xchg16(StgWord x, StgWord val);
StgWord hs_xchg32(StgWord x, StgWord val);
StgWord64 hs_xchg64(StgWord x, StgWord64 val);

/* libraries/ghc-internal/cbits/bswap.c */
StgWord16 hs_bswap16(StgWord16 x);
StgWord32 hs_bswap32(StgWord32 x);
StgWord64 hs_bswap64(StgWord64 x);

/* libraries/ghc-internal/cbits/bitrev.c
This was done as part of issue #16164.
See Note [Bit reversal primop] for more details about the implementation.*/
StgWord hs_bitrev8(StgWord x);
StgWord16 hs_bitrev16(StgWord16 x);
StgWord32 hs_bitrev32(StgWord32 x);
StgWord64 hs_bitrev64(StgWord64 x);

/* libraries/ghc-internal/cbits/longlong.c */
#if WORD_SIZE_IN_BITS < 64
StgInt hs_eq64 (StgWord64 a, StgWord64 b);
StgInt hs_ne64 (StgWord64 a, StgWord64 b);
StgInt hs_gtWord64 (StgWord64 a, StgWord64 b);
StgInt hs_geWord64 (StgWord64 a, StgWord64 b);
StgInt hs_ltWord64 (StgWord64 a, StgWord64 b);
StgInt hs_leWord64 (StgWord64 a, StgWord64 b);
StgInt hs_gtInt64 (StgInt64 a, StgInt64 b);
StgInt hs_geInt64 (StgInt64 a, StgInt64 b);
StgInt hs_ltInt64 (StgInt64 a, StgInt64 b);
StgInt hs_leInt64 (StgInt64 a, StgInt64 b);
StgInt64 hs_neg64       (StgInt64 a);
StgWord64 hs_add64      (StgWord64 a, StgWord64 b);
StgWord64 hs_sub64      (StgWord64 a, StgWord64 b);
StgWord64 hs_mul64      (StgWord64 a, StgWord64 b);
StgWord64 hs_remWord64  (StgWord64 a, StgWord64 b);
StgWord64 hs_quotWord64 (StgWord64 a, StgWord64 b);
StgInt64 hs_remInt64    (StgInt64 a, StgInt64 b);
StgInt64 hs_quotInt64   (StgInt64 a, StgInt64 b);
StgWord64 hs_and64      (StgWord64 a, StgWord64 b);
StgWord64 hs_or64       (StgWord64 a, StgWord64 b);
StgWord64 hs_xor64      (StgWord64 a, StgWord64 b);
StgWord64 hs_not64      (StgWord64 a);
StgWord64 hs_uncheckedShiftL64   (StgWord64 a, StgInt b);
StgWord64 hs_uncheckedShiftRL64  (StgWord64 a, StgInt b);
StgInt64  hs_uncheckedIShiftRA64 (StgInt64 a,  StgInt b);
StgInt64  hs_intToInt64    (StgInt    i);
StgInt    hs_int64ToInt    (StgInt64  i);
StgWord64 hs_wordToWord64  (StgWord   w);
StgWord   hs_word64ToWord  (StgWord64 w);
#endif

/* libraries/ghc-internal/cbits/pdep.c */
StgWord64 hs_pdep64(StgWord64 src, StgWord64 mask);
StgWord hs_pdep32(StgWord src, StgWord mask);
StgWord hs_pdep16(StgWord src, StgWord mask);
StgWord hs_pdep8(StgWord src, StgWord mask);

/* libraries/ghc-internal/cbits/pext.c */
StgWord64 hs_pext64(StgWord64 src, StgWord64 mask);
StgWord hs_pext32(StgWord src, StgWord mask);
StgWord hs_pext16(StgWord src, StgWord mask);
StgWord hs_pext8(StgWord src, StgWord mask);

/* libraries/ghc-internal/cbits/popcnt.c */
StgWord hs_popcnt8(StgWord x);
StgWord hs_popcnt16(StgWord x);
StgWord hs_popcnt32(StgWord x);
StgWord hs_popcnt64(StgWord64 x);
StgWord hs_popcnt(StgWord x);

/* libraries/ghc-internal/cbits/word2float.c */
StgFloat hs_word2float32(StgWord x);
StgDouble hs_word2float64(StgWord x);

/* libraries/ghc-internal/cbits/clz.c */
StgWord hs_clz8(StgWord x);
StgWord hs_clz16(StgWord x);
StgWord hs_clz32(StgWord x);
StgWord hs_clz64(StgWord64 x);

/* libraries/ghc-internal/cbits/ctz.c */
StgWord hs_ctz8(StgWord x);
StgWord hs_ctz16(StgWord x);
StgWord hs_ctz32(StgWord x);
StgWord hs_ctz64(StgWord64 x);

/* bitcasts, instead of creating a new C file we static inline these here. We
 * use __builtin_memcpy instead of memcpy from string.h to avoid function
 * prototype conflicts that occur in the C backend with the inclusion of
 * string.h*/
static inline StgFloat hs_bitcastword2float(StgWord32 x) {
    StgFloat dest;
    __builtin_memcpy(&dest, &x, sizeof(StgFloat));
    return dest;
}

static inline StgDouble hs_bitcastword642double(StgWord64 x) {
    StgDouble dest;
    __builtin_memcpy(&dest, &x, sizeof(StgDouble));
    return dest;
}

static inline StgWord32 hs_bitcastfloat2word(StgFloat x) {
    StgWord32 dest;
    __builtin_memcpy(&dest, &x, sizeof(StgWord32));
    return dest;
}
static inline StgWord64 hs_bitcastdouble2word64(StgDouble x) {
    StgWord64 dest;
    __builtin_memcpy(&dest, &x, sizeof(StgWord64));
    return dest;
}
