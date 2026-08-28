#include <HsFFI.h>

/* Runtime x86 CPU feature detection backing GHC.Driver.CpuFeatures
   (-march=native).

   The bit positions in the returned mask are GHC's own; the enum below
   must match the constructor order of X86CpuFeature in
   GHC.Driver.CpuFeatures.

   The __builtin_cpu_* calls below compile to references to libgcc/compiler-rt
   symbols (__cpu_model & co.), which GHCi's runtime linker can only resolve
   because the RTS exports them: see RTS_X86_CPU_MODEL_SYMBOLS in
   rts/RtsSymbols.c and keep its guards in sync with this file's. */

enum {
  GHC_X86_FEAT_SSE2 = 0,
  GHC_X86_FEAT_SSE3,
  GHC_X86_FEAT_SSSE3,
  GHC_X86_FEAT_SSE4_1,
  GHC_X86_FEAT_SSE4_2,
  GHC_X86_FEAT_AVX,
  GHC_X86_FEAT_AVX2,
  GHC_X86_FEAT_AVX512F,
  GHC_X86_FEAT_AVX512BW,
  GHC_X86_FEAT_AVX512CD,
  GHC_X86_FEAT_AVX512DQ,
  GHC_X86_FEAT_AVX512VL,
  GHC_X86_FEAT_BMI1,
  GHC_X86_FEAT_BMI2,
  GHC_X86_FEAT_FMA,
  GHC_X86_FEAT_GFNI
};

#if (defined(__i386__) || defined(__x86_64__)) \
    && (defined(__GNUC__) || defined(__clang__))

/* GFNI joined the __builtin_cpu_supports feature lists later than the
   other features here (GCC 11; compiler-rt's cpu_model in LLVM 12).
   Under an older bootstrap compiler we conservatively report no GFNI. */
#if (defined(__clang_major__) && __clang_major__ >= 12) \
    || (!defined(__clang__) && defined(__GNUC__) && __GNUC__ >= 11)
#define GHC_HAS_CPU_SUPPORTS_GFNI 1
#else
#define GHC_HAS_CPU_SUPPORTS_GFNI 0
#endif

HsWord64 ghc_detect_x86_cpu_features(void)
{
  __builtin_cpu_init();

  HsWord64 feats = 0;

  /* __builtin_cpu_supports returns the feature's (nonzero) mask bit,
     not 0/1, so it must be normalized before shifting. */
#define GHC_CPU_FEATURE(name, bit) \
  if (__builtin_cpu_supports(name)) { feats |= ((HsWord64)1 << (bit)); }

  GHC_CPU_FEATURE("sse2",     GHC_X86_FEAT_SSE2)
  GHC_CPU_FEATURE("sse3",     GHC_X86_FEAT_SSE3)
  GHC_CPU_FEATURE("ssse3",    GHC_X86_FEAT_SSSE3)
  GHC_CPU_FEATURE("sse4.1",   GHC_X86_FEAT_SSE4_1)
  GHC_CPU_FEATURE("sse4.2",   GHC_X86_FEAT_SSE4_2)
  GHC_CPU_FEATURE("avx",      GHC_X86_FEAT_AVX)
  GHC_CPU_FEATURE("avx2",     GHC_X86_FEAT_AVX2)
  GHC_CPU_FEATURE("avx512f",  GHC_X86_FEAT_AVX512F)
  GHC_CPU_FEATURE("avx512bw", GHC_X86_FEAT_AVX512BW)
  GHC_CPU_FEATURE("avx512cd", GHC_X86_FEAT_AVX512CD)
  GHC_CPU_FEATURE("avx512dq", GHC_X86_FEAT_AVX512DQ)
  GHC_CPU_FEATURE("avx512vl", GHC_X86_FEAT_AVX512VL)
  GHC_CPU_FEATURE("bmi",      GHC_X86_FEAT_BMI1)
  GHC_CPU_FEATURE("bmi2",     GHC_X86_FEAT_BMI2)

  /* FMA is usable only when the OS saves the AVX register state.  However
     compiler-rt's cpu_model/x86.c reports "fma" from the raw CPUID bit. */
  if (__builtin_cpu_supports("fma") && __builtin_cpu_supports("avx")) {
    feats |= ((HsWord64)1 << GHC_X86_FEAT_FMA);
  }

#if GHC_HAS_CPU_SUPPORTS_GFNI
  GHC_CPU_FEATURE("gfni",     GHC_X86_FEAT_GFNI)
#endif

#undef GHC_CPU_FEATURE

  return feats;
}

#else

/* Non-x86 host, or a compiler without __builtin_cpu_supports (e.g. MSVC):
   report no features. */
HsWord64 ghc_detect_x86_cpu_features(void)
{
  return 0;
}

#endif
