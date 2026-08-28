#include <HsFFI.h>

/* Runtime x86 CPU feature detection backing GHC.Platform.HostCpuFeatures
   (-march=native).

   The __builtin_cpu_* calls below compile to references to libgcc/compiler-rt
   symbols (__cpu_model & co.), which GHCi's runtime linker can only resolve
   because the RTS exports them: see RTS_X86_CPU_MODEL_SYMBOLS in
   rts/RtsSymbols.c and keep its guards in sync with this file's. */

#include "HostCpuFeaturesX86.h"

#if defined(__i386__) || defined(__x86_64__)

#define SUPPORTS(name) __builtin_cpu_supports(name)

/* FMA is usable only when the OS saves the AVX register state.  However
   compiler-rt's cpu_model/x86.c reports "fma" from the raw CPUID bit. */
#define SUPPORTS_FMA (SUPPORTS("fma") && SUPPORTS("avx"))

/* GFNI joined the __builtin_cpu_supports feature lists later than the
   other features here (GCC 11; compiler-rt's cpu_model in LLVM 12).
   Under an older bootstrap compiler we conservatively report no GFNI. */
#if (defined(__clang_major__) && __clang_major__ >= 12) \
    || (!defined(__clang__) && defined(__GNUC__) && __GNUC__ >= 11)
#define SUPPORTS_GFNI SUPPORTS("gfni")
#else
#define SUPPORTS_GFNI 0
#endif

HsWord64 ghc_host_x86_cpu_feature_mask(void)
{
  __builtin_cpu_init();

  HsWord64 feats = 0;
  int bit = 0;

  /* The conditions are only truthy, not 0/1: __builtin_cpu_supports
     returns the feature's mask bit. */
#define GHC_X86_PROBE(feat, cond) \
  if (cond) { feats |= (HsWord64)1 << bit; } \
  bit++;

  GHC_X86_PROBED_FEATURES(GHC_X86_PROBE)

#undef GHC_X86_PROBE

  return feats;
}

#else

/* Non-x86 host: report no features. */
HsWord64 ghc_host_x86_cpu_feature_mask(void)
{
  return 0;
}

#endif
