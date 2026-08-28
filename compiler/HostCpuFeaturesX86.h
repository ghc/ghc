/* The x86 features that -march=native probes. Each row pairs a CpuFeature
   constructor with the C condition (see cbits/host_cpu_features_x86.c) that
   detects the feature on the host. SSE and SSE2 are not probed: they are
   GHC's x86 baseline.

   Row i is bit i of the mask that ghc_host_x86_cpu_feature_mask returns and
   GHC.Platform.HostCpuFeatures decodes, so the table can hold at most 64
   rows. */
#define GHC_X86_PROBED_FEATURES(F) \
  F(Feat_SSE3,     SUPPORTS("sse3"))     \
  F(Feat_SSSE3,    SUPPORTS("ssse3"))    \
  F(Feat_SSE4,     SUPPORTS("sse4.1"))   \
  F(Feat_SSE4_2,   SUPPORTS("sse4.2"))   \
  F(Feat_AVX,      SUPPORTS("avx"))      \
  F(Feat_AVX2,     SUPPORTS("avx2"))     \
  F(Feat_AVX512F,  SUPPORTS("avx512f"))  \
  F(Feat_AVX512BW, SUPPORTS("avx512bw")) \
  F(Feat_AVX512CD, SUPPORTS("avx512cd")) \
  F(Feat_AVX512DQ, SUPPORTS("avx512dq")) \
  F(Feat_AVX512VL, SUPPORTS("avx512vl")) \
  F(Feat_BMI,      SUPPORTS("bmi"))      \
  F(Feat_BMI2,     SUPPORTS("bmi2"))     \
  F(Feat_FMA,      SUPPORTS_FMA)         \
  F(Feat_GFNI,     SUPPORTS_GFNI)
