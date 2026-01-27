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

StgWord hs_atomic_add8(StgWord x, StgWord val)
{
  return __atomic_fetch_add((volatile StgWord8 *) x, (StgWord8) val, __ATOMIC_SEQ_CST);
}

StgWord hs_atomic_add16(StgWord x, StgWord val)
{
  return __atomic_fetch_add((volatile StgWord16 *) x, (StgWord16) val, __ATOMIC_SEQ_CST);
}

StgWord hs_atomic_add32(StgWord x, StgWord val)
{
  return __atomic_fetch_add((volatile StgWord32 *) x, (StgWord32) val, __ATOMIC_SEQ_CST);
}

StgWord64 hs_atomic_add64(StgWord x, StgWord64 val)
{
  return __atomic_fetch_add((volatile StgWord64 *) x, val, __ATOMIC_SEQ_CST);
}

// FetchSubByteArrayOp_Int

StgWord hs_atomic_sub8(StgWord x, StgWord val)
{
  return __atomic_fetch_sub((volatile StgWord8 *) x, (StgWord8) val, __ATOMIC_SEQ_CST);
}

StgWord hs_atomic_sub16(StgWord x, StgWord val)
{
  return __atomic_fetch_sub((volatile StgWord16 *) x, (StgWord16) val, __ATOMIC_SEQ_CST);
}

StgWord hs_atomic_sub32(StgWord x, StgWord val)
{
  return __atomic_fetch_sub((volatile StgWord32 *) x, (StgWord32) val, __ATOMIC_SEQ_CST);
}

StgWord64 hs_atomic_sub64(StgWord x, StgWord64 val)
{
  return __atomic_fetch_sub((volatile StgWord64 *) x, val, __ATOMIC_SEQ_CST);
}

// FetchAndByteArrayOp_Int

StgWord hs_atomic_and8(StgWord x, StgWord val)
{
  return __atomic_fetch_and((volatile StgWord8 *) x, (StgWord8) val, __ATOMIC_SEQ_CST);
}

StgWord hs_atomic_and16(StgWord x, StgWord val)
{
  return __atomic_fetch_and((volatile StgWord16 *) x, (StgWord16) val, __ATOMIC_SEQ_CST);
}

StgWord hs_atomic_and32(StgWord x, StgWord val)
{
  return __atomic_fetch_and((volatile StgWord32 *) x, (StgWord32) val, __ATOMIC_SEQ_CST);
}

StgWord64 hs_atomic_and64(StgWord x, StgWord64 val)
{
  return __atomic_fetch_and((volatile StgWord64 *) x, val, __ATOMIC_SEQ_CST);
}

StgWord hs_atomic_nand8(StgWord x, StgWord val)
{
  return __atomic_fetch_nand((volatile StgWord8 *) x, (StgWord8) val, __ATOMIC_SEQ_CST);
}

StgWord hs_atomic_nand16(StgWord x, StgWord val)
{
  return __atomic_fetch_nand((volatile StgWord16 *) x, (StgWord16) val, __ATOMIC_SEQ_CST);
}

StgWord hs_atomic_nand32(StgWord x, StgWord val)
{
  return __atomic_fetch_nand((volatile StgWord32 *) x, (StgWord32) val, __ATOMIC_SEQ_CST);
}

StgWord64 hs_atomic_nand64(StgWord x, StgWord64 val)
{
  return __atomic_fetch_nand((volatile StgWord64 *) x, val, __ATOMIC_SEQ_CST);
}

#pragma GCC diagnostic pop

// FetchOrByteArrayOp_Int

StgWord hs_atomic_or8(StgWord x, StgWord val)
{
  return __atomic_fetch_or((volatile StgWord8 *) x, (StgWord8) val, __ATOMIC_SEQ_CST);
}

StgWord hs_atomic_or16(StgWord x, StgWord val)
{
  return __atomic_fetch_or((volatile StgWord16 *) x, (StgWord16) val, __ATOMIC_SEQ_CST);
}

StgWord hs_atomic_or32(StgWord x, StgWord val)
{
  return __atomic_fetch_or((volatile StgWord32 *) x, (StgWord32) val, __ATOMIC_SEQ_CST);
}

StgWord64 hs_atomic_or64(StgWord x, StgWord64 val)
{
  return __atomic_fetch_or((volatile StgWord64 *) x, val, __ATOMIC_SEQ_CST);
}

// FetchXorByteArrayOp_Int

StgWord hs_atomic_xor8(StgWord x, StgWord val)
{
  return __atomic_fetch_xor((volatile StgWord8 *) x, (StgWord8) val, __ATOMIC_SEQ_CST);
}

StgWord hs_atomic_xor16(StgWord x, StgWord val)
{
  return __atomic_fetch_xor((volatile StgWord16 *) x, (StgWord16) val, __ATOMIC_SEQ_CST);
}

StgWord hs_atomic_xor32(StgWord x, StgWord val)
{
  return __atomic_fetch_xor((volatile StgWord32 *) x, (StgWord32) val, __ATOMIC_SEQ_CST);
}

StgWord64 hs_atomic_xor64(StgWord x, StgWord64 val)
{
  return __atomic_fetch_xor((volatile StgWord64 *) x, val, __ATOMIC_SEQ_CST);
}

// CasByteArrayOp_Int

StgWord hs_cmpxchg8(StgWord x, StgWord old, StgWord new)
{
  StgWord8 expected = (StgWord8) old;
  __atomic_compare_exchange_n((StgWord8 *) x, &expected, (StgWord8) new, false, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
  return expected;
}

StgWord hs_cmpxchg16(StgWord x, StgWord old, StgWord new)
{
  StgWord16 expected = (StgWord16) old;
  __atomic_compare_exchange_n((StgWord16 *) x, &expected, (StgWord16) new, false, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
  return expected;
}

StgWord hs_cmpxchg32(StgWord x, StgWord old, StgWord new)
{
  StgWord32 expected = (StgWord32) old;
  __atomic_compare_exchange_n((StgWord32 *) x, &expected, (StgWord32) new, false, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
  return expected;
}

StgWord64 hs_cmpxchg64(StgWord x, StgWord64 old, StgWord64 new)
{
  StgWord64 expected = (StgWord64) old;
  __atomic_compare_exchange_n((StgWord64 *) x, &expected, (StgWord64) new, false, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
  return expected;
}

// Atomic exchange operations

StgWord hs_xchg8(StgWord x, StgWord val)
{
  return (StgWord) __atomic_exchange_n((StgWord8 *) x, (StgWord8) val, __ATOMIC_SEQ_CST);
}

StgWord hs_xchg16(StgWord x, StgWord val)
{
  return (StgWord) __atomic_exchange_n((StgWord16 *)x, (StgWord16) val, __ATOMIC_SEQ_CST);
}

StgWord hs_xchg32(StgWord x, StgWord val)
{
  return (StgWord) __atomic_exchange_n((StgWord32 *) x, (StgWord32) val, __ATOMIC_SEQ_CST);
}

//GCC provides this even on 32bit, but StgWord is still 32 bits.
StgWord64 hs_xchg64(StgWord x, StgWord64 val)
{
  return (StgWord64) __atomic_exchange_n((StgWord64 *) x, (StgWord64) val, __ATOMIC_SEQ_CST);
}

// AtomicReadByteArrayOp_Int
// Implies a full memory barrier (see compiler/GHC/Builtin/primops.txt.pp)
// __ATOMIC_SEQ_CST: Full barrier in both directions (hoisting and sinking
// of code) and synchronizes with acquire loads and release stores in
// all threads.

StgWord hs_atomicread8(StgWord x)
{
  return __atomic_load_n((StgWord8 *) x, __ATOMIC_SEQ_CST);
}

StgWord hs_atomicread16(StgWord x)
{
  return __atomic_load_n((StgWord16 *) x, __ATOMIC_SEQ_CST);
}

StgWord hs_atomicread32(StgWord x)
{
  return __atomic_load_n((StgWord32 *) x, __ATOMIC_SEQ_CST);
}

StgWord64 hs_atomicread64(StgWord x)
{
  return __atomic_load_n((StgWord64 *) x, __ATOMIC_SEQ_CST);
}

// AtomicWriteByteArrayOp_Int
// Implies a full memory barrier (see compiler/GHC/Builtin/primops.txt.pp)
// __ATOMIC_SEQ_CST: Full barrier (see hs_atomicread8 above).

void hs_atomicwrite8(StgWord x, StgWord val)
{
  __atomic_store_n((StgWord8 *) x, (StgWord8) val, __ATOMIC_SEQ_CST);
}

void hs_atomicwrite16(StgWord x, StgWord val)
{
  __atomic_store_n((StgWord16 *) x, (StgWord16) val, __ATOMIC_SEQ_CST);
}

void hs_atomicwrite32(StgWord x, StgWord val)
{
  __atomic_store_n((StgWord32 *) x, (StgWord32) val, __ATOMIC_SEQ_CST);
}

void hs_atomicwrite64(StgWord x, StgWord64 val)
{
  __atomic_store_n((StgWord64 *) x, (StgWord64) val, __ATOMIC_SEQ_CST);
}

#endif
