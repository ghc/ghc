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

extern StgWord hs_atomic_add8(volatile StgWord8 *x, StgWord val);
StgWord
hs_atomic_add8(volatile StgWord8 *x, StgWord val)
{
  return __sync_fetch_and_add(x, (StgWord8) val);
}

extern StgWord hs_atomic_add16(volatile StgWord16 *x, StgWord val);
StgWord
hs_atomic_add16(volatile StgWord16 *x, StgWord val)
{
  return __sync_fetch_and_add(x, (StgWord16) val);
}

extern StgWord hs_atomic_add32(volatile StgWord32 *x, StgWord val);
StgWord
hs_atomic_add32(volatile StgWord32 *x, StgWord val)
{
  return __sync_fetch_and_add(x, (StgWord32) val);
}

#if WORD_SIZE_IN_BITS == 64
extern StgWord64 hs_atomic_add64(volatile StgWord64 *x, StgWord64 val);
StgWord64
hs_atomic_add64(volatile StgWord64 *x, StgWord64 val)
{
  return __sync_fetch_and_add(x, val);
}
#endif

// FetchSubByteArrayOp_Int

extern StgWord hs_atomic_sub8(volatile StgWord8 *x, StgWord val);
StgWord
hs_atomic_sub8(volatile StgWord8 *x, StgWord val)
{
  return __sync_fetch_and_sub(x, (StgWord8) val);
}

extern StgWord hs_atomic_sub16(volatile StgWord16 *x, StgWord val);
StgWord
hs_atomic_sub16(volatile StgWord16 *x, StgWord val)
{
  return __sync_fetch_and_sub(x, (StgWord16) val);
}

extern StgWord hs_atomic_sub32(volatile StgWord32 *x, StgWord val);
StgWord
hs_atomic_sub32(volatile StgWord32 *x, StgWord val)
{
  return __sync_fetch_and_sub(x, (StgWord32) val);
}

#if WORD_SIZE_IN_BITS == 64
extern StgWord64 hs_atomic_sub64(volatile StgWord64 *x, StgWord64 val);
StgWord64
hs_atomic_sub64(volatile StgWord64 *x, StgWord64 val)
{
  return __sync_fetch_and_sub(x, val);
}
#endif

// FetchAndByteArrayOp_Int

extern StgWord hs_atomic_and8(volatile StgWord8 *x, StgWord val);
StgWord
hs_atomic_and8(volatile StgWord8 *x, StgWord val)
{
  return __sync_fetch_and_and(x, (StgWord8) val);
}

extern StgWord hs_atomic_and16(volatile StgWord16 *x, StgWord val);
StgWord
hs_atomic_and16(volatile StgWord16 *x, StgWord val)
{
  return __sync_fetch_and_and(x, (StgWord16) val);
}

extern StgWord hs_atomic_and32(volatile StgWord32 *x, StgWord val);
StgWord
hs_atomic_and32(volatile StgWord32 *x, StgWord val)
{
  return __sync_fetch_and_and(x, (StgWord32) val);
}

#if WORD_SIZE_IN_BITS == 64
extern StgWord64 hs_atomic_and64(volatile StgWord64 *x, StgWord64 val);
StgWord64
hs_atomic_and64(volatile StgWord64 *x, StgWord64 val)
{
  return __sync_fetch_and_and(x, val);
}
#endif

// FetchNandByteArrayOp_Int

// Workaround for http://llvm.org/bugs/show_bug.cgi?id=8842
#define CAS_NAND(x, val)                                            \
  {                                                                 \
    __typeof__ (*(x)) tmp = *(x);                                   \
    while (!__sync_bool_compare_and_swap(x, tmp, ~(tmp & (val)))) { \
      tmp = *(x);                                                   \
    }                                                               \
    return tmp;                                                     \
  }

extern StgWord hs_atomic_nand8(volatile StgWord8 *x, StgWord val);
StgWord
hs_atomic_nand8(volatile StgWord8 *x, StgWord val)
{
#ifdef __clang__
  CAS_NAND(x, (StgWord8) val)
#else
  return __sync_fetch_and_nand(x, (StgWord8) val);
#endif
}

extern StgWord hs_atomic_nand16(volatile StgWord16 *x, StgWord val);
StgWord
hs_atomic_nand16(volatile StgWord16 *x, StgWord val)
{
#ifdef __clang__
  CAS_NAND(x, (StgWord16) val);
#else
  return __sync_fetch_and_nand(x, (StgWord16) val);
#endif
}

extern StgWord hs_atomic_nand32(volatile StgWord32 *x, StgWord val);
StgWord
hs_atomic_nand32(volatile StgWord32 *x, StgWord val)
{
#ifdef __clang__
  CAS_NAND(x, (StgWord32) val);
#else
  return __sync_fetch_and_nand(x, (StgWord32) val);
#endif
}

#if WORD_SIZE_IN_BITS == 64
extern StgWord64 hs_atomic_nand64(volatile StgWord64 *x, StgWord64 val);
StgWord64
hs_atomic_nand64(volatile StgWord64 *x, StgWord64 val)
{
#ifdef __clang__
  CAS_NAND(x, val);
#else
  return __sync_fetch_and_nand(x, val);
#endif
}
#endif

// FetchOrByteArrayOp_Int

extern StgWord hs_atomic_or8(volatile StgWord8 *x, StgWord val);
StgWord
hs_atomic_or8(volatile StgWord8 *x, StgWord val)
{
  return __sync_fetch_and_or(x, (StgWord8) val);
}

extern StgWord hs_atomic_or16(volatile StgWord16 *x, StgWord val);
StgWord
hs_atomic_or16(volatile StgWord16 *x, StgWord val)
{
  return __sync_fetch_and_or(x, (StgWord16) val);
}

extern StgWord hs_atomic_or32(volatile StgWord32 *x, StgWord val);
StgWord
hs_atomic_or32(volatile StgWord32 *x, StgWord val)
{
  return __sync_fetch_and_or(x, (StgWord32) val);
}

#if WORD_SIZE_IN_BITS == 64
extern StgWord64 hs_atomic_or64(volatile StgWord64 *x, StgWord64 val);
StgWord64
hs_atomic_or64(volatile StgWord64 *x, StgWord64 val)
{
  return __sync_fetch_and_or(x, val);
}
#endif

// FetchXorByteArrayOp_Int

extern StgWord hs_atomic_xor8(volatile StgWord8 *x, StgWord val);
StgWord
hs_atomic_xor8(volatile StgWord8 *x, StgWord val)
{
  return __sync_fetch_and_xor(x, (StgWord8) val);
}

extern StgWord hs_atomic_xor16(volatile StgWord16 *x, StgWord val);
StgWord
hs_atomic_xor16(volatile StgWord16 *x, StgWord val)
{
  return __sync_fetch_and_xor(x, (StgWord16) val);
}

extern StgWord hs_atomic_xor32(volatile StgWord32 *x, StgWord val);
StgWord
hs_atomic_xor32(volatile StgWord32 *x, StgWord val)
{
  return __sync_fetch_and_xor(x, (StgWord32) val);
}

#if WORD_SIZE_IN_BITS == 64
extern StgWord64 hs_atomic_xor64(volatile StgWord64 *x, StgWord64 val);
StgWord64
hs_atomic_xor64(volatile StgWord64 *x, StgWord64 val)
{
  return __sync_fetch_and_xor(x, val);
}
#endif

// CasByteArrayOp_Int

extern StgWord hs_cmpxchg8(volatile StgWord8 *x, StgWord old, StgWord new);
StgWord
hs_cmpxchg8(volatile StgWord8 *x, StgWord old, StgWord new)
{
  return __sync_val_compare_and_swap(x, (StgWord8) old, (StgWord8) new);
}

extern StgWord hs_cmpxchg16(volatile StgWord16 *x, StgWord old, StgWord new);
StgWord
hs_cmpxchg16(volatile StgWord16 *x, StgWord old, StgWord new)
{
  return __sync_val_compare_and_swap(x, (StgWord16) old, (StgWord16) new);
}

extern StgWord hs_cmpxchg32(volatile StgWord32 *x, StgWord old, StgWord new);
StgWord
hs_cmpxchg32(volatile StgWord32 *x, StgWord old, StgWord new)
{
  return __sync_val_compare_and_swap(x, (StgWord32) old, (StgWord32) new);
}

#if WORD_SIZE_IN_BITS == 64
extern StgWord hs_cmpxchg64(volatile StgWord64 *x, StgWord64 old, StgWord64 new);
StgWord
hs_cmpxchg64(volatile StgWord64 *x, StgWord64 old, StgWord64 new)
{
  return __sync_val_compare_and_swap(x, old, new);
}
#endif

// AtomicReadByteArrayOp_Int

extern StgWord hs_atomicread8(volatile StgWord8 *x);
StgWord
hs_atomicread8(volatile StgWord8 *x)
{
  return *x;
}

extern StgWord hs_atomicread16(volatile StgWord16 *x);
StgWord
hs_atomicread16(volatile StgWord16 *x)
{
  return *x;
}

extern StgWord hs_atomicread32(volatile StgWord32 *x);
StgWord
hs_atomicread32(volatile StgWord32 *x)
{
  return *x;
}

extern StgWord64 hs_atomicread64(volatile StgWord64 *x);
StgWord64
hs_atomicread64(volatile StgWord64 *x)
{
  return *x;
}

// AtomicWriteByteArrayOp_Int

extern void hs_atomicwrite8(volatile StgWord8 *x, StgWord val);
void
hs_atomicwrite8(volatile StgWord8 *x, StgWord val)
{
  *x = (StgWord8) val;
}

extern void hs_atomicwrite16(volatile StgWord16 *x, StgWord val);
void
hs_atomicwrite16(volatile StgWord16 *x, StgWord val)
{
  *x = (StgWord16) val;
}

extern void hs_atomicwrite32(volatile StgWord32 *x, StgWord val);
void
hs_atomicwrite32(volatile StgWord32 *x, StgWord val)
{
  *x = (StgWord32) val;
}

extern void hs_atomicwrite64(volatile StgWord64 *x, StgWord64 val);
void
hs_atomicwrite64(volatile StgWord64 *x, StgWord64 val)
{
  *x = (StgWord64) val;
}
