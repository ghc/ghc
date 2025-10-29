#if defined(__SSE2__)
#include <immintrin.h>

__m128d sub(__m128d x, __m128d y)
{
  return _mm_sub_pd(x,y);
}

__m128d add7(__m128d x1, __m128d x2, __m128d x3, __m128d x4, __m128d x5, __m128d x6, __m128d x7)
{
  return _mm_add_pd(x1,_mm_add_pd(x2,_mm_add_pd(x3,_mm_add_pd(x4,_mm_add_pd(x5,_mm_add_pd(x6, x7))))));
}

#elif defined(__aarch64__)
#include <arm_neon.h>

float64x2_t sub(float64x2_t x, float64x2_t y)
{
  return vsubq_f64(x, y);
}

float64x2_t add7(float64x2_t x1, float64x2_t x2, float64x2_t x3, float64x2_t x4, float64x2_t x5, float64x2_t x6, float64x2_t x7)
{
  return vaddq_f64(x1, vaddq_f64(x2, vaddq_f64(x3, vaddq_f64(x4, vaddq_f64(x5, vaddq_f64(x6, x7))))));
}

#endif
