-- BMI and BMI2 are not listed: GHC defines no macros for them.
#if defined(__SSE3__)
feat sse3
#endif
#if defined(__SSSE3__)
feat ssse3
#endif
#if defined(__SSE4_1__)
feat sse4.1
#endif
#if defined(__SSE4_2__)
feat sse4.2
#endif
#if defined(__AVX__)
feat avx
#endif
#if defined(__AVX2__)
feat avx2
#endif
#if defined(__AVX512F__)
feat avx512f
#endif
#if defined(__AVX512BW__)
feat avx512bw
#endif
#if defined(__AVX512CD__)
feat avx512cd
#endif
#if defined(__AVX512DQ__)
feat avx512dq
#endif
#if defined(__AVX512VL__)
feat avx512vl
#endif
#if defined(__FMA__)
feat fma
#endif
#if defined(__GFNI__)
feat gfni
#endif
