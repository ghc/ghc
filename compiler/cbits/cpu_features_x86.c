#include <HsFFI.h>
#include <stdint.h>

#if defined(_MSC_VER) && (defined(_M_IX86) || defined(_M_X64))
#include <immintrin.h>
#include <intrin.h>
#endif

#if !defined(_MSC_VER) && (defined(__i386__) || defined(__x86_64__))
#include <cpuid.h>
#endif

#if defined(__APPLE__) && (defined(__i386__) || defined(__x86_64__))
#include <sys/sysctl.h>
#endif

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

#define SET_FEAT(mask, bit) ((mask) |= ((HsWord64)1ULL << (bit)))

static int ghc_cpuid_count(uint32_t leaf, uint32_t subleaf,
                           uint32_t *a, uint32_t *b, uint32_t *c, uint32_t *d)
{
#if defined(_MSC_VER) && (defined(_M_IX86) || defined(_M_X64))
  int regs[4];
  __cpuidex(regs, (int)leaf, (int)subleaf);
  *a = (uint32_t)regs[0];
  *b = (uint32_t)regs[1];
  *c = (uint32_t)regs[2];
  *d = (uint32_t)regs[3];
  return 1;
#elif defined(__i386__) || defined(__x86_64__)
  return __get_cpuid_count(leaf, subleaf, a, b, c, d);
#else
  (void)leaf;
  (void)subleaf;
  (void)a;
  (void)b;
  (void)c;
  (void)d;
  return 0;
#endif
}

static uint64_t ghc_xgetbv0(void)
{
#if defined(_MSC_VER) && (defined(_M_IX86) || defined(_M_X64))
  return (uint64_t)_xgetbv(0);
#elif defined(__i386__) || defined(__x86_64__)
  uint32_t eax, edx;
  __asm__ volatile(".byte 0x0f, 0x01, 0xd0" /* xgetbv */
                   : "=a"(eax), "=d"(edx)
                   : "c"(0));
  return ((uint64_t)edx << 32) | (uint64_t)eax;
#else
  return 0;
#endif
}

#if defined(__APPLE__) && (defined(__i386__) || defined(__x86_64__))
/* Query a macOS CPU-capability sysctl, e.g. "hw.optional.avx512f". */
static int ghc_macos_sysctl_flag(const char *name)
{
  int result = 0;
  size_t len = sizeof(result);
  if (sysctlbyname(name, &result, &len, NULL, 0) != 0) {
    return 0;
  }
  return result != 0;
}
#endif

HsWord64 ghc_detect_x86_cpu_features(void)
{
  HsWord64 feats = 0;

#if defined(_M_IX86) || defined(_M_X64) || defined(__i386__) || defined(__x86_64__)
  uint32_t a, b, c, d;
  uint32_t max_basic = 0;

  if (!ghc_cpuid_count(0, 0, &a, &b, &c, &d)) {
    return 0;
  }
  max_basic = a;
  if (max_basic < 1) {
    return 0;
  }

  ghc_cpuid_count(1, 0, &a, &b, &c, &d);

  {
    int has_sse2    = !!(d & (1u << 26));
    int has_sse3    = !!(c & (1u << 0));
    int has_ssse3   = !!(c & (1u << 9));
    int has_sse4_1  = !!(c & (1u << 19));
    int has_sse4_2  = !!(c & (1u << 20));
    int has_fma_hw  = !!(c & (1u << 12));
    int has_avx_hw  = !!(c & (1u << 28));
    int has_osxsave = !!(c & (1u << 27));

    int avx_usable = 0;
    int avx512_usable = 0;

    if (has_osxsave) {
      uint64_t xcr0 = ghc_xgetbv0();
      avx_usable = ((xcr0 & 0x6u) == 0x6u);      /* XMM + YMM state */
      avx512_usable = ((xcr0 & 0xE6u) == 0xE6u); /* XMM+YMM+opmask+ZMM */
    }

#if defined(__APPLE__)
    /* On x86_64 macOS the kernel enables AVX-512 XSAVE state lazily: XCR0
       reads back with the opmask/ZMM bits clear until a process first faults
       on an AVX-512 instruction, so the XCR0 check above is a false negative
       on AVX-512-capable Macs. Use the OS feature query instead. Checking
       AVX512F alone suffices here; the AVX-512 sub-features (BW/CD/DQ/VL) are
       still decoded from CPUID leaf 7 below.

       Refs:
         https://zenn.dev/mod_poppo/articles/detect-processor-features-x86?locale=en#notes-on-detecting-avx-512-on-macos
         https://github.com/minoki/haskell-cpu-features */
    avx512_usable = ghc_macos_sysctl_flag("hw.optional.avx512f");
#endif

    if (has_sse2) {
      SET_FEAT(feats, GHC_X86_FEAT_SSE2);
    }
    if (has_sse3) {
      SET_FEAT(feats, GHC_X86_FEAT_SSE3);
    }
    if (has_ssse3) {
      SET_FEAT(feats, GHC_X86_FEAT_SSSE3);
    }
    if (has_sse4_1) {
      SET_FEAT(feats, GHC_X86_FEAT_SSE4_1);
    }
    if (has_sse4_2) {
      SET_FEAT(feats, GHC_X86_FEAT_SSE4_2);
    }
    if (has_avx_hw && avx_usable) {
      SET_FEAT(feats, GHC_X86_FEAT_AVX);
    }
    if (has_fma_hw && avx_usable) {
      SET_FEAT(feats, GHC_X86_FEAT_FMA);
    }

    if (max_basic >= 7 && ghc_cpuid_count(7, 0, &a, &b, &c, &d)) {
      int has_bmi1     = !!(b & (1u << 3));
      int has_avx2_hw  = !!(b & (1u << 5));
      int has_bmi2     = !!(b & (1u << 8));
      int has_avx512f  = !!(b & (1u << 16));
      int has_avx512dq = !!(b & (1u << 17));
      int has_avx512cd = !!(b & (1u << 28));
      int has_avx512bw = !!(b & (1u << 30));
      int has_avx512vl = !!(b & (1u << 31));
      int has_gfni     = !!(c & (1u << 8));

      if (has_bmi1) {
        SET_FEAT(feats, GHC_X86_FEAT_BMI1);
      }
      if (has_bmi2) {
        SET_FEAT(feats, GHC_X86_FEAT_BMI2);
      }
      if (avx_usable && has_avx2_hw) {
        SET_FEAT(feats, GHC_X86_FEAT_AVX2);
      }

      if (avx512_usable && has_avx512f) {
        SET_FEAT(feats, GHC_X86_FEAT_AVX512F);
        if (has_avx512bw) {
          SET_FEAT(feats, GHC_X86_FEAT_AVX512BW);
        }
        if (has_avx512cd) {
          SET_FEAT(feats, GHC_X86_FEAT_AVX512CD);
        }
        if (has_avx512dq) {
          SET_FEAT(feats, GHC_X86_FEAT_AVX512DQ);
        }
        if (has_avx512vl) {
          SET_FEAT(feats, GHC_X86_FEAT_AVX512VL);
        }
      }

      if (has_gfni) {
        SET_FEAT(feats, GHC_X86_FEAT_GFNI);
      }
    }
  }
#endif

  return feats;
}
