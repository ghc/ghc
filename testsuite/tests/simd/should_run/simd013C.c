
#include <immintrin.h>

__m128d sub(__m128d x, __m128d y)
{
  return _mm_sub_pd(x,y);
}

__m128d add7(__m128d x1, __m128d x2, __m128d x3, __m128d x4, __m128d x5, __m128d x6, __m128d x7)
{
  return _mm_add_pd(x1,_mm_add_pd(x2,_mm_add_pd(x3,_mm_add_pd(x4,_mm_add_pd(x5,_mm_add_pd(x6, x7))))));
}
