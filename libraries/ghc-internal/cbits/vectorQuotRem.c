#if defined(__SSE2__)
#include <stdint.h>
#include <string.h>
#include <emmintrin.h>

/* There are no SIMD instructions for integer quot/rem in x86.

If we can rely on GCC extensions, we can write it as follows:

typedef int8_t int8x16_t __attribute__((vector_size(16)));
int8x16_t hs_quotInt8X16(int8x16_t x, int8x16_t y)
{
    return x / y;
}

*/

__m128i hs_quotInt8X16(__m128i xx, __m128i yy)
{
  int8_t x[16], y[16];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  int8_t z0 = x[0] / y[0];
  int8_t z1 = x[1] / y[1];
  int8_t z2 = x[2] / y[2];
  int8_t z3 = x[3] / y[3];
  int8_t z4 = x[4] / y[4];
  int8_t z5 = x[5] / y[5];
  int8_t z6 = x[6] / y[6];
  int8_t z7 = x[7] / y[7];
  int8_t z8 = x[8] / y[8];
  int8_t z9 = x[9] / y[9];
  int8_t z10 = x[10] / y[10];
  int8_t z11 = x[11] / y[11];
  int8_t z12 = x[12] / y[12];
  int8_t z13 = x[13] / y[13];
  int8_t z14 = x[14] / y[14];
  int8_t z15 = x[15] / y[15];
  return _mm_set_epi8(z15, z14, z13, z12, z11, z10, z9, z8, z7, z6, z5, z4, z3, z2, z1, z0);
}

__m128i hs_quotInt16X8(__m128i xx, __m128i yy)
{
  int16_t x[8], y[8];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  int16_t z0 = x[0] / y[0];
  int16_t z1 = x[1] / y[1];
  int16_t z2 = x[2] / y[2];
  int16_t z3 = x[3] / y[3];
  int16_t z4 = x[4] / y[4];
  int16_t z5 = x[5] / y[5];
  int16_t z6 = x[6] / y[6];
  int16_t z7 = x[7] / y[7];
  return _mm_set_epi16(z7, z6, z5, z4, z3, z2, z1, z0);
}

__m128i hs_quotInt32X4(__m128i xx, __m128i yy)
{
  int32_t x[4], y[4];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  int32_t z0 = x[0] / y[0];
  int32_t z1 = x[1] / y[1];
  int32_t z2 = x[2] / y[2];
  int32_t z3 = x[3] / y[3];
  return _mm_set_epi32(z3, z2, z1, z0);
}

__m128i hs_quotInt64X2(__m128i xx, __m128i yy)
{
  int64_t x[2], y[2];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  int64_t z0 = x[0] / y[0];
  int64_t z1 = x[1] / y[1];
  return _mm_set_epi64x(z1, z0);
}

__m128i hs_quotWord8X16(__m128i xx, __m128i yy)
{
  uint8_t x[16], y[16];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  uint8_t z0 = x[0] / y[0];
  uint8_t z1 = x[1] / y[1];
  uint8_t z2 = x[2] / y[2];
  uint8_t z3 = x[3] / y[3];
  uint8_t z4 = x[4] / y[4];
  uint8_t z5 = x[5] / y[5];
  uint8_t z6 = x[6] / y[6];
  uint8_t z7 = x[7] / y[7];
  uint8_t z8 = x[8] / y[8];
  uint8_t z9 = x[9] / y[9];
  uint8_t z10 = x[10] / y[10];
  uint8_t z11 = x[11] / y[11];
  uint8_t z12 = x[12] / y[12];
  uint8_t z13 = x[13] / y[13];
  uint8_t z14 = x[14] / y[14];
  uint8_t z15 = x[15] / y[15];
  return _mm_set_epi8(z15, z14, z13, z12, z11, z10, z9, z8, z7, z6, z5, z4, z3, z2, z1, z0);
}

__m128i hs_quotWord16X8(__m128i xx, __m128i yy)
{
  uint16_t x[8], y[8];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  uint16_t z0 = x[0] / y[0];
  uint16_t z1 = x[1] / y[1];
  uint16_t z2 = x[2] / y[2];
  uint16_t z3 = x[3] / y[3];
  uint16_t z4 = x[4] / y[4];
  uint16_t z5 = x[5] / y[5];
  uint16_t z6 = x[6] / y[6];
  uint16_t z7 = x[7] / y[7];
  return _mm_set_epi16(z7, z6, z5, z4, z3, z2, z1, z0);
}

__m128i hs_quotWord32X4(__m128i xx, __m128i yy)
{
  uint32_t x[4], y[4];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  uint32_t z0 = x[0] / y[0];
  uint32_t z1 = x[1] / y[1];
  uint32_t z2 = x[2] / y[2];
  uint32_t z3 = x[3] / y[3];
  return _mm_set_epi32(z3, z2, z1, z0);
}

__m128i hs_quotWord64X2(__m128i xx, __m128i yy)
{
  uint64_t x[2], y[2];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  uint64_t z0 = x[0] / y[0];
  uint64_t z1 = x[1] / y[1];
  return _mm_set_epi64x(z1, z0);
}

__m128i hs_remInt8X16(__m128i xx, __m128i yy)
{
  int8_t x[16], y[16];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  int8_t z0 = x[0] % y[0];
  int8_t z1 = x[1] % y[1];
  int8_t z2 = x[2] % y[2];
  int8_t z3 = x[3] % y[3];
  int8_t z4 = x[4] % y[4];
  int8_t z5 = x[5] % y[5];
  int8_t z6 = x[6] % y[6];
  int8_t z7 = x[7] % y[7];
  int8_t z8 = x[8] % y[8];
  int8_t z9 = x[9] % y[9];
  int8_t z10 = x[10] % y[10];
  int8_t z11 = x[11] % y[11];
  int8_t z12 = x[12] % y[12];
  int8_t z13 = x[13] % y[13];
  int8_t z14 = x[14] % y[14];
  int8_t z15 = x[15] % y[15];
  return _mm_set_epi8(z15, z14, z13, z12, z11, z10, z9, z8, z7, z6, z5, z4, z3, z2, z1, z0);
}

__m128i hs_remInt16X8(__m128i xx, __m128i yy)
{
  int16_t x[8], y[8];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  int16_t z0 = x[0] % y[0];
  int16_t z1 = x[1] % y[1];
  int16_t z2 = x[2] % y[2];
  int16_t z3 = x[3] % y[3];
  int16_t z4 = x[4] % y[4];
  int16_t z5 = x[5] % y[5];
  int16_t z6 = x[6] % y[6];
  int16_t z7 = x[7] % y[7];
  return _mm_set_epi16(z7, z6, z5, z4, z3, z2, z1, z0);
}

__m128i hs_remInt32X4(__m128i xx, __m128i yy)
{
  int32_t x[4], y[4];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  int32_t z0 = x[0] % y[0];
  int32_t z1 = x[1] % y[1];
  int32_t z2 = x[2] % y[2];
  int32_t z3 = x[3] % y[3];
  return _mm_set_epi32(z3, z2, z1, z0);
}

__m128i hs_remInt64X2(__m128i xx, __m128i yy)
{
  int64_t x[2], y[2];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  int64_t z0 = x[0] % y[0];
  int64_t z1 = x[1] % y[1];
  return _mm_set_epi64x(z1, z0);
}

__m128i hs_remWord8X16(__m128i xx, __m128i yy)
{
  uint8_t x[16], y[16];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  uint8_t z0 = x[0] % y[0];
  uint8_t z1 = x[1] % y[1];
  uint8_t z2 = x[2] % y[2];
  uint8_t z3 = x[3] % y[3];
  uint8_t z4 = x[4] % y[4];
  uint8_t z5 = x[5] % y[5];
  uint8_t z6 = x[6] % y[6];
  uint8_t z7 = x[7] % y[7];
  uint8_t z8 = x[8] % y[8];
  uint8_t z9 = x[9] % y[9];
  uint8_t z10 = x[10] % y[10];
  uint8_t z11 = x[11] % y[11];
  uint8_t z12 = x[12] % y[12];
  uint8_t z13 = x[13] % y[13];
  uint8_t z14 = x[14] % y[14];
  uint8_t z15 = x[15] % y[15];
  return _mm_set_epi8(z15, z14, z13, z12, z11, z10, z9, z8, z7, z6, z5, z4, z3, z2, z1, z0);
}

__m128i hs_remWord16X8(__m128i xx, __m128i yy)
{
  uint16_t x[8], y[8];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  uint16_t z0 = x[0] % y[0];
  uint16_t z1 = x[1] % y[1];
  uint16_t z2 = x[2] % y[2];
  uint16_t z3 = x[3] % y[3];
  uint16_t z4 = x[4] % y[4];
  uint16_t z5 = x[5] % y[5];
  uint16_t z6 = x[6] % y[6];
  uint16_t z7 = x[7] % y[7];
  return _mm_set_epi16(z7, z6, z5, z4, z3, z2, z1, z0);
}

__m128i hs_remWord32X4(__m128i xx, __m128i yy)
{
  uint32_t x[4], y[4];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  uint32_t z0 = x[0] % y[0];
  uint32_t z1 = x[1] % y[1];
  uint32_t z2 = x[2] % y[2];
  uint32_t z3 = x[3] % y[3];
  return _mm_set_epi32(z3, z2, z1, z0);
}

__m128i hs_remWord64X2(__m128i xx, __m128i yy)
{
  uint64_t x[2], y[2];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  uint64_t z0 = x[0] % y[0];
  uint64_t z1 = x[1] % y[1];
  return _mm_set_epi64x(z1, z0);
}

#endif
