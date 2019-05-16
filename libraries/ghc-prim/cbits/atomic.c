#if !defined(arm_HOST_ARCH)
#include "Rts.h"

// Fallbacks for atomic primops on byte arrays. The builtins used
// below are supported on both GCC and LLVM.
//
// Ideally these function would take StgWord8, StgWord16, etc but
// older GCC versions incorrectly assume that the register that the
// argument is passed in has been zero extended, which is incorrect
// according to the ABI and is not what GHC does when it generates
// calls to these functions.

// FetchAddByteArrayOp_Int

extern StgWord hs_atomic_add8(StgWord x, StgWord val);
StgWord
hs_atomic_add8(StgWord x, StgWord val)
{
  return __sync_fetch_and_add((volatile StgWord8 *) x, (StgWord8) val);
}

extern StgWord hs_atomic_add16(StgWord x, StgWord val);
StgWord
hs_atomic_add16(StgWord x, StgWord val)
{
  return __sync_fetch_and_add((volatile StgWord16 *) x, (StgWord16) val);
}

extern StgWord hs_atomic_add32(StgWord x, StgWord val);
StgWord
hs_atomic_add32(StgWord x, StgWord val)
{
  return __sync_fetch_and_add((volatile StgWord32 *) x, (StgWord32) val);
}

#if WORD_SIZE_IN_BITS == 64
extern StgWord64 hs_atomic_add64(StgWord x, StgWord64 val);
StgWord64
hs_atomic_add64(StgWord x, StgWord64 val)
{
  return __sync_fetch_and_add((volatile StgWord64 *) x, val);
}
#endif

// FetchSubByteArrayOp_Int

extern StgWord hs_atomic_sub8(StgWord x, StgWord val);
StgWord
hs_atomic_sub8(StgWord x, StgWord val)
{
  return __sync_fetch_and_sub((volatile StgWord8 *) x, (StgWord8) val);
}

extern StgWord hs_atomic_sub16(StgWord x, StgWord val);
StgWord
hs_atomic_sub16(StgWord x, StgWord val)
{
  return __sync_fetch_and_sub((volatile StgWord16 *) x, (StgWord16) val);
}

extern StgWord hs_atomic_sub32(StgWord x, StgWord val);
StgWord
hs_atomic_sub32(StgWord x, StgWord val)
{
  return __sync_fetch_and_sub((volatile StgWord32 *) x, (StgWord32) val);
}

#if WORD_SIZE_IN_BITS == 64
extern StgWord64 hs_atomic_sub64(StgWord x, StgWord64 val);
StgWord64
hs_atomic_sub64(StgWord x, StgWord64 val)
{
  return __sync_fetch_and_sub((volatile StgWord64 *) x, val);
}
#endif

// FetchAndByteArrayOp_Int

extern StgWord hs_atomic_and8(StgWord x, StgWord val);
StgWord
hs_atomic_and8(StgWord x, StgWord val)
{
  return __sync_fetch_and_and((volatile StgWord8 *) x, (StgWord8) val);
}

extern StgWord hs_atomic_and16(StgWord x, StgWord val);
StgWord
hs_atomic_and16(StgWord x, StgWord val)
{
  return __sync_fetch_and_and((volatile StgWord16 *) x, (StgWord16) val);
}

extern StgWord hs_atomic_and32(StgWord x, StgWord val);
StgWord
hs_atomic_and32(StgWord x, StgWord val)
{
  return __sync_fetch_and_and((volatile StgWord32 *) x, (StgWord32) val);
}

#if WORD_SIZE_IN_BITS == 64
extern StgWord64 hs_atomic_and64(StgWord x, StgWord64 val);
StgWord64
hs_atomic_and64(StgWord x, StgWord64 val)
{
  return __sync_fetch_and_and((volatile StgWord64 *) x, val);
}
#endif

// FetchNandByteArrayOp_Int

// Note [__sync_fetch_and_nand usage]
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//
// The __sync_fetch_and_nand builtin is a bit of a disaster. It was introduced
// in GCC long ago with silly semantics. Specifically:
//
//    *ptr = ~(tmp & value)
//
// Clang introduced the builtin with the same semantics.
//
// In GCC 4.4 the operation's semantics were rightly changed to,
//
//    *ptr = ~tmp & value
//
// and the -Wsync-nand warning was added warning users of the operation about
// the change.
//
// Clang took this change as a reason to remove support for the
// builtin in 2010. Then, in 2014 Clang re-added support with the new
// semantics. However, the warning flag was given a different name
// (-Wsync-fetch-and-nand-semantics-changed) for added fun.
//
// Consequently, we are left with a bit of a mess: GHC requires GCC >4.4
// (enforced by the FP_GCC_VERSION autoconf check), so we thankfully don't need
// to support the operation's older broken semantics. However, we need to take
// care to explicitly disable -Wsync-nand wherever possible, lest the build
// fails with -Werror.  Furthermore, we need to emulate the operation when
// building with some Clang versions (shipped by some Mac OS X releases) which
// lack support for the builtin.
//
// In the words of Bob Dylan: everything is broken.
//
// See also:
//
//  * https://bugs.llvm.org/show_bug.cgi?id=8842
//  * https://ghc.haskell.org/trac/ghc/ticket/9678
//

#define CAS_NAND(x, val)                                            \
  {                                                                 \
    __typeof__ (*(x)) tmp = *(x);                                   \
    while (!__sync_bool_compare_and_swap(x, tmp, ~(tmp & (val)))) { \
      tmp = *(x);                                                   \
    }                                                               \
    return tmp;                                                     \
  }

// N.B. __has_builtin is only provided by clang
#if !defined(__has_builtin)
#define __has_builtin(x) 0
#endif

#if defined(__clang__) && !__has_builtin(__sync_fetch_and_nand)
#define USE_SYNC_FETCH_AND_NAND 0
#else
#define USE_SYNC_FETCH_AND_NAND 1
#endif

// Otherwise this fails with -Werror
#pragma GCC diagnostic push
#if defined(__clang__)
#pragma GCC diagnostic ignored "-Wsync-fetch-and-nand-semantics-changed"
#elif defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wsync-nand"
#endif

extern StgWord hs_atomic_nand8(StgWord x, StgWord val);
StgWord
hs_atomic_nand8(StgWord x, StgWord val)
{
#if USE_SYNC_FETCH_AND_NAND
  return __sync_fetch_and_nand((volatile StgWord8 *) x, (StgWord8) val);
#else
  CAS_NAND((volatile StgWord8 *) x, (StgWord8) val)
#endif
}

extern StgWord hs_atomic_nand16(StgWord x, StgWord val);
StgWord
hs_atomic_nand16(StgWord x, StgWord val)
{
#if USE_SYNC_FETCH_AND_NAND
  return __sync_fetch_and_nand((volatile StgWord16 *) x, (StgWord16) val);
#else
  CAS_NAND((volatile StgWord16 *) x, (StgWord16) val);
#endif
}

extern StgWord hs_atomic_nand32(StgWord x, StgWord val);
StgWord
hs_atomic_nand32(StgWord x, StgWord val)
{
#if USE_SYNC_FETCH_AND_NAND
  return __sync_fetch_and_nand((volatile StgWord32 *) x, (StgWord32) val);
#else
  CAS_NAND((volatile StgWord32 *) x, (StgWord32) val);
#endif
}

#if WORD_SIZE_IN_BITS == 64
extern StgWord64 hs_atomic_nand64(StgWord x, StgWord64 val);
StgWord64
hs_atomic_nand64(StgWord x, StgWord64 val)
{
#if USE_SYNC_FETCH_AND_NAND
  return __sync_fetch_and_nand((volatile StgWord64 *) x, val);
#else
  CAS_NAND((volatile StgWord64 *) x, val);
#endif
}
#endif

#pragma GCC diagnostic pop

// FetchOrByteArrayOp_Int

extern StgWord hs_atomic_or8(StgWord x, StgWord val);
StgWord
hs_atomic_or8(StgWord x, StgWord val)
{
  return __sync_fetch_and_or((volatile StgWord8 *) x, (StgWord8) val);
}

extern StgWord hs_atomic_or16(StgWord x, StgWord val);
StgWord
hs_atomic_or16(StgWord x, StgWord val)
{
  return __sync_fetch_and_or((volatile StgWord16 *) x, (StgWord16) val);
}

extern StgWord hs_atomic_or32(StgWord x, StgWord val);
StgWord
hs_atomic_or32(StgWord x, StgWord val)
{
  return __sync_fetch_and_or((volatile StgWord32 *) x, (StgWord32) val);
}

#if WORD_SIZE_IN_BITS == 64
extern StgWord64 hs_atomic_or64(StgWord x, StgWord64 val);
StgWord64
hs_atomic_or64(StgWord x, StgWord64 val)
{
  return __sync_fetch_and_or((volatile StgWord64 *) x, val);
}
#endif

// FetchXorByteArrayOp_Int

extern StgWord hs_atomic_xor8(StgWord x, StgWord val);
StgWord
hs_atomic_xor8(StgWord x, StgWord val)
{
  return __sync_fetch_and_xor((volatile StgWord8 *) x, (StgWord8) val);
}

extern StgWord hs_atomic_xor16(StgWord x, StgWord val);
StgWord
hs_atomic_xor16(StgWord x, StgWord val)
{
  return __sync_fetch_and_xor((volatile StgWord16 *) x, (StgWord16) val);
}

extern StgWord hs_atomic_xor32(StgWord x, StgWord val);
StgWord
hs_atomic_xor32(StgWord x, StgWord val)
{
  return __sync_fetch_and_xor((volatile StgWord32 *) x, (StgWord32) val);
}

#if WORD_SIZE_IN_BITS == 64
extern StgWord64 hs_atomic_xor64(StgWord x, StgWord64 val);
StgWord64
hs_atomic_xor64(StgWord x, StgWord64 val)
{
  return __sync_fetch_and_xor((volatile StgWord64 *) x, val);
}
#endif

// CasByteArrayOp_Int

extern StgWord hs_cmpxchg8(StgWord x, StgWord old, StgWord new);
StgWord
hs_cmpxchg8(StgWord x, StgWord old, StgWord new)
{
  return __sync_val_compare_and_swap((volatile StgWord8 *) x, (StgWord8) old, (StgWord8) new);
}

extern StgWord hs_cmpxchg16(StgWord x, StgWord old, StgWord new);
StgWord
hs_cmpxchg16(StgWord x, StgWord old, StgWord new)
{
  return __sync_val_compare_and_swap((volatile StgWord16 *) x, (StgWord16) old, (StgWord16) new);
}

extern StgWord hs_cmpxchg32(StgWord x, StgWord old, StgWord new);
StgWord
hs_cmpxchg32(StgWord x, StgWord old, StgWord new)
{
  return __sync_val_compare_and_swap((volatile StgWord32 *) x, (StgWord32) old, (StgWord32) new);
}

#if WORD_SIZE_IN_BITS == 64
extern StgWord hs_cmpxchg64(StgWord x, StgWord64 old, StgWord64 new);
StgWord
hs_cmpxchg64(StgWord x, StgWord64 old, StgWord64 new)
{
  return __sync_val_compare_and_swap((volatile StgWord64 *) x, old, new);
}
#endif

// AtomicReadByteArrayOp_Int
// Implies a full memory barrier (see compiler/prelude/primops.txt.pp)
// __ATOMIC_SEQ_CST: Full barrier in both directions (hoisting and sinking
// of code) and synchronizes with acquire loads and release stores in
// all threads.
//
// When we lack C11 atomics support we emulate these using the old GCC __sync
// primitives which the GCC documentation claims "usually" implies a full
// barrier.

extern StgWord hs_atomicread8(StgWord x);
StgWord
hs_atomicread8(StgWord x)
{
#if HAVE_C11_ATOMICS
  return __atomic_load_n((StgWord8 *) x, __ATOMIC_SEQ_CST);
#else
  return __sync_add_and_fetch((StgWord8 *) x, 0);
#endif
}

extern StgWord hs_atomicread16(StgWord x);
StgWord
hs_atomicread16(StgWord x)
{
#if HAVE_C11_ATOMICS
  return __atomic_load_n((StgWord16 *) x, __ATOMIC_SEQ_CST);
#else
  return __sync_add_and_fetch((StgWord16 *) x, 0);
#endif
}

extern StgWord hs_atomicread32(StgWord x);
StgWord
hs_atomicread32(StgWord x)
{
#if HAVE_C11_ATOMICS
  return __atomic_load_n((StgWord32 *) x, __ATOMIC_SEQ_CST);
#else
  return __sync_add_and_fetch((StgWord32 *) x, 0);
#endif
}

extern StgWord64 hs_atomicread64(StgWord x);
StgWord64
hs_atomicread64(StgWord x)
{
#if HAVE_C11_ATOMICS
  return __atomic_load_n((StgWord64 *) x, __ATOMIC_SEQ_CST);
#else
  return __sync_add_and_fetch((StgWord64 *) x, 0);
#endif
}

// AtomicWriteByteArrayOp_Int
// Implies a full memory barrier (see compiler/prelude/primops.txt.pp)
// __ATOMIC_SEQ_CST: Full barrier (see hs_atomicread8 above).

extern void hs_atomicwrite8(StgWord x, StgWord val);
void
hs_atomicwrite8(StgWord x, StgWord val)
{
#if HAVE_C11_ATOMICS
  __atomic_store_n((StgWord8 *) x, (StgWord8) val, __ATOMIC_SEQ_CST);
#else
  while (!__sync_bool_compare_and_swap((StgWord8 *) x, *(StgWord8 *) x, (StgWord8) val));
#endif
}

extern void hs_atomicwrite16(StgWord x, StgWord val);
void
hs_atomicwrite16(StgWord x, StgWord val)
{
#if HAVE_C11_ATOMICS
  __atomic_store_n((StgWord16 *) x, (StgWord16) val, __ATOMIC_SEQ_CST);
#else
  while (!__sync_bool_compare_and_swap((StgWord16 *) x, *(StgWord16 *) x, (StgWord16) val));
#endif
}

extern void hs_atomicwrite32(StgWord x, StgWord val);
void
hs_atomicwrite32(StgWord x, StgWord val)
{
#if HAVE_C11_ATOMICS
  __atomic_store_n((StgWord32 *) x, (StgWord32) val, __ATOMIC_SEQ_CST);
#else
  while (!__sync_bool_compare_and_swap((StgWord32 *) x, *(StgWord32 *) x, (StgWord32) val));
#endif
}

extern void hs_atomicwrite64(StgWord x, StgWord64 val);
void
hs_atomicwrite64(StgWord x, StgWord64 val)
{
#if HAVE_C11_ATOMICS
  __atomic_store_n((StgWord64 *) x, (StgWord64) val, __ATOMIC_SEQ_CST);
#else
  while (!__sync_bool_compare_and_swap((StgWord64 *) x, *(StgWord64 *) x, (StgWord64) val));
#endif
}
#endif
