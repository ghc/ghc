#if defined(__SSE2__)
#include <stdint.h>
#include <string.h>
#include <emmintrin.h>

// SSE2 does not provide min/max instructions for Int64X2/Word64X2.
// In fact, even comparison instructions require SSE4.2.
// Therefore, for SSE2, we implement the min/max operations in C.
// If performance is critical, your choices are:
//   * use the LLVM backend, or
//   * enable SSE4.2, or
//   * implement min/max in NCG.

__m128i hs_minInt64X2(__m128i xx, __m128i yy)
{
  int64_t x[2], y[2];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  int64_t z0 = x[0] < y[0] ? x[0] : y[0];
  int64_t z1 = x[1] < y[1] ? x[1] : y[1];
  return _mm_set_epi64x(z1, z0);
}

__m128i hs_maxInt64X2(__m128i xx, __m128i yy)
{
  int64_t x[2], y[2];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  int64_t z0 = x[0] < y[0] ? y[0] : x[0];
  int64_t z1 = x[1] < y[1] ? y[1] : x[1];
  return _mm_set_epi64x(z1, z0);
}

__m128i hs_minWord64X2(__m128i xx, __m128i yy)
{
  uint64_t x[2], y[2];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  uint64_t z0 = x[0] < y[0] ? x[0] : y[0];
  uint64_t z1 = x[1] < y[1] ? x[1] : y[1];
  return _mm_set_epi64x(z1, z0);
}

__m128i hs_maxWord64X2(__m128i xx, __m128i yy)
{
  uint64_t x[2], y[2];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  uint64_t z0 = x[0] < y[0] ? y[0] : x[0];
  uint64_t z1 = x[1] < y[1] ? y[1] : x[1];
  return _mm_set_epi64x(z1, z0);
}

#endif
