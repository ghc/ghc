#include <HsFFI.h>

/* Host x86 CPU feature detection for GHC.Platform.HostCpuFeatures
   (-march=native).

   The RTS has to export the libgcc/compiler-rt symbols that the
   __builtin_cpu_* calls refer to: see RTS_X86_CPU_MODEL_SYMBOLS in
   rts/RtsSymbols.c. */

#include "HostCpuFeaturesX86.h"

#if defined(__i386__) || defined(__x86_64__)

#define SUPPORTS(name) __builtin_cpu_supports(name)

/* FMA is usable only when the OS saves the AVX register state, but
   compiler-rt's cpu_model/x86.c reports "fma" from the raw CPUID bit. */
#define SUPPORTS_FMA (SUPPORTS("fma") && SUPPORTS("avx"))

/* __builtin_cpu_supports accepts "gfni" from GCC 8 and LLVM 8 on, and reads
   it from __cpu_features2, which libgcc provides only from GCC 8 on. */
#if !defined(__clang__) && defined(__GNUC__) && __GNUC__ >= 8
/* Weak, so that programs using the ghc library from a binary distribution
   can link against a libgcc older than GCC 8. The declaration also makes
   the builtin's own reference weak. */
extern unsigned int __cpu_features2[] __attribute__((weak));
#define SUPPORTS_GFNI (__cpu_features2 != 0 && SUPPORTS("gfni"))
#elif defined(__clang__) \
   && __clang_major__ >= (defined(__apple_build_version__) ? 11 : 8)
/* Apple clang numbers its releases separately: 11 is its first based on
   LLVM 8. Clang marks its reference dso_local, so a weak declaration would
   not help: an older libgcc fails the link. */
#define SUPPORTS_GFNI SUPPORTS("gfni")
#else
#define SUPPORTS_GFNI 0
#endif

HsWord64 ghc_host_x86_cpu_feature_mask(void)
{
  __builtin_cpu_init();

  HsWord64 mask = 0;
  int bit = 0;

#define GHC_X86_PROBE(feat, cond) \
  if (cond) { mask |= (HsWord64)1 << bit; } \
  bit++;

  GHC_X86_PROBED_FEATURES(GHC_X86_PROBE)

#undef GHC_X86_PROBE

  return mask;
}

#else

HsWord64 ghc_host_x86_cpu_feature_mask(void)
{
  return 0;
}

#endif
