#include "Rts.h"
#include <stdint.h>
#include <string.h>
#if defined(__SSE2__)
#include <emmintrin.h>

/* There are no SIMD instructions for integer quot/rem in x86.

If we can rely on GCC extensions, we can write it as follows:

typedef int8_t int8x16_t __attribute__((vector_size(16)));
int8x16_t hs_quotInt8X16(int8x16_t x, int8x16_t y)
{
    return x / y;
}

*/

v128 hs_quotInt8X16(v128 xx, v128 yy)
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
  return (v128) _mm_set_epi8(z15, z14, z13, z12, z11, z10, z9, z8, z7, z6, z5, z4, z3, z2, z1, z0);
}

v128 hs_quotInt16X8(v128 xx, v128 yy)
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
  return (v128) _mm_set_epi16(z7, z6, z5, z4, z3, z2, z1, z0);
}

v128 hs_quotInt32X4(v128 xx, v128 yy)
{
  int32_t x[4], y[4];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  int32_t z0 = x[0] / y[0];
  int32_t z1 = x[1] / y[1];
  int32_t z2 = x[2] / y[2];
  int32_t z3 = x[3] / y[3];
  return (v128) _mm_set_epi32(z3, z2, z1, z0);
}

v128 hs_quotInt64X2(v128 xx, v128 yy)
{
  int64_t x[2], y[2];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  int64_t z0 = x[0] / y[0];
  int64_t z1 = x[1] / y[1];
  return (v128) _mm_set_epi64x(z1, z0);
}

v128 hs_quotWord8X16(v128 xx, v128 yy)
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
  return (v128) _mm_set_epi8(z15, z14, z13, z12, z11, z10, z9, z8, z7, z6, z5, z4, z3, z2, z1, z0);
}

v128 hs_quotWord16X8(v128 xx, v128 yy)
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
  return (v128) _mm_set_epi16(z7, z6, z5, z4, z3, z2, z1, z0);
}

v128 hs_quotWord32X4(v128 xx, v128 yy)
{
  uint32_t x[4], y[4];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  uint32_t z0 = x[0] / y[0];
  uint32_t z1 = x[1] / y[1];
  uint32_t z2 = x[2] / y[2];
  uint32_t z3 = x[3] / y[3];
  return (v128) _mm_set_epi32(z3, z2, z1, z0);
}

v128 hs_quotWord64X2(v128 xx, v128 yy)
{
  uint64_t x[2], y[2];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  uint64_t z0 = x[0] / y[0];
  uint64_t z1 = x[1] / y[1];
  return (v128) _mm_set_epi64x(z1, z0);
}

v128 hs_remInt8X16(v128 xx, v128 yy)
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
  return (v128) _mm_set_epi8(z15, z14, z13, z12, z11, z10, z9, z8, z7, z6, z5, z4, z3, z2, z1, z0);
}

v128 hs_remInt16X8(v128 xx, v128 yy)
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
  return (v128) _mm_set_epi16(z7, z6, z5, z4, z3, z2, z1, z0);
}

v128 hs_remInt32X4(v128 xx, v128 yy)
{
  int32_t x[4], y[4];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  int32_t z0 = x[0] % y[0];
  int32_t z1 = x[1] % y[1];
  int32_t z2 = x[2] % y[2];
  int32_t z3 = x[3] % y[3];
  return (v128) _mm_set_epi32(z3, z2, z1, z0);
}

v128 hs_remInt64X2(v128 xx, v128 yy)
{
  int64_t x[2], y[2];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  int64_t z0 = x[0] % y[0];
  int64_t z1 = x[1] % y[1];
  return (v128) _mm_set_epi64x(z1, z0);
}

v128 hs_remWord8X16(v128 xx, v128 yy)
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
  return (v128) _mm_set_epi8(z15, z14, z13, z12, z11, z10, z9, z8, z7, z6, z5, z4, z3, z2, z1, z0);
}

v128 hs_remWord16X8(v128 xx, v128 yy)
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
  return (v128) _mm_set_epi16(z7, z6, z5, z4, z3, z2, z1, z0);
}

v128 hs_remWord32X4(v128 xx, v128 yy)
{
  uint32_t x[4], y[4];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  uint32_t z0 = x[0] % y[0];
  uint32_t z1 = x[1] % y[1];
  uint32_t z2 = x[2] % y[2];
  uint32_t z3 = x[3] % y[3];
  return (v128) _mm_set_epi32(z3, z2, z1, z0);
}

v128 hs_remWord64X2(v128 xx, v128 yy)
{
  uint64_t x[2], y[2];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  uint64_t z0 = x[0] % y[0];
  uint64_t z1 = x[1] % y[1];
  return (v128) _mm_set_epi64x(z1, z0);
}

#elif defined(__aarch64__)
#include <arm_neon.h>

/* There are no SIMD instructions for integer quot/rem in AArch64.

If we can rely on GCC extensions, we can write it as follows:

typedef int8_t int8x16_t __attribute__((vector_size(16)));
int8x16_t hs_quotInt8X16(int8x16_t x, int8x16_t y)
{
    return x / y;
}

*/

v128 hs_quotInt8X16(v128 xx, v128 yy)
{
  int8_t x[16], y[16];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  int8x16_t v = vdupq_n_s8(0);
  v = vsetq_lane_s8(x[0] / y[0], v, 0);
  v = vsetq_lane_s8(x[1] / y[1], v, 1);
  v = vsetq_lane_s8(x[2] / y[2], v, 2);
  v = vsetq_lane_s8(x[3] / y[3], v, 3);
  v = vsetq_lane_s8(x[4] / y[4], v, 4);
  v = vsetq_lane_s8(x[5] / y[5], v, 5);
  v = vsetq_lane_s8(x[6] / y[6], v, 6);
  v = vsetq_lane_s8(x[7] / y[7], v, 7);
  v = vsetq_lane_s8(x[8] / y[8], v, 8);
  v = vsetq_lane_s8(x[9] / y[9], v, 9);
  v = vsetq_lane_s8(x[10] / y[10], v, 10);
  v = vsetq_lane_s8(x[11] / y[11], v, 11);
  v = vsetq_lane_s8(x[12] / y[12], v, 12);
  v = vsetq_lane_s8(x[13] / y[13], v, 13);
  v = vsetq_lane_s8(x[14] / y[14], v, 14);
  v = vsetq_lane_s8(x[15] / y[15], v, 15);
  return (v128) v;
}

v128 hs_quotInt16X8(v128 xx, v128 yy)
{
  int16_t x[8], y[8];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  int16x8_t v = vdupq_n_s16(0);
  v = vsetq_lane_s16(x[0] / y[0], v, 0);
  v = vsetq_lane_s16(x[1] / y[1], v, 1);
  v = vsetq_lane_s16(x[2] / y[2], v, 2);
  v = vsetq_lane_s16(x[3] / y[3], v, 3);
  v = vsetq_lane_s16(x[4] / y[4], v, 4);
  v = vsetq_lane_s16(x[5] / y[5], v, 5);
  v = vsetq_lane_s16(x[6] / y[6], v, 6);
  v = vsetq_lane_s16(x[7] / y[7], v, 7);
  return (v128) v;
}

v128 hs_quotInt32X4(v128 xx, v128 yy)
{
  int32_t x[4], y[4];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  int32x4_t v = vdupq_n_s32(0);
  v = vsetq_lane_s32(x[0] / y[0], v, 0);
  v = vsetq_lane_s32(x[1] / y[1], v, 1);
  v = vsetq_lane_s32(x[2] / y[2], v, 2);
  v = vsetq_lane_s32(x[3] / y[3], v, 3);
  return (v128) v;
}

v128 hs_quotInt64X2(v128 xx, v128 yy)
{
  int64_t x[2], y[2];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  int64x2_t v = vdupq_n_s64(0);
  v = vsetq_lane_s64(x[0] / y[0], v, 0);
  v = vsetq_lane_s64(x[1] / y[1], v, 1);
  return (v128) v;
}

v128 hs_quotWord8X16(v128 xx, v128 yy)
{
  uint8_t x[16], y[16];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  uint8x16_t v = vdupq_n_u8(0);
  v = vsetq_lane_u8(x[0] / y[0], v, 0);
  v = vsetq_lane_u8(x[1] / y[1], v, 1);
  v = vsetq_lane_u8(x[2] / y[2], v, 2);
  v = vsetq_lane_u8(x[3] / y[3], v, 3);
  v = vsetq_lane_u8(x[4] / y[4], v, 4);
  v = vsetq_lane_u8(x[5] / y[5], v, 5);
  v = vsetq_lane_u8(x[6] / y[6], v, 6);
  v = vsetq_lane_u8(x[7] / y[7], v, 7);
  v = vsetq_lane_u8(x[8] / y[8], v, 8);
  v = vsetq_lane_u8(x[9] / y[9], v, 9);
  v = vsetq_lane_u8(x[10] / y[10], v, 10);
  v = vsetq_lane_u8(x[11] / y[11], v, 11);
  v = vsetq_lane_u8(x[12] / y[12], v, 12);
  v = vsetq_lane_u8(x[13] / y[13], v, 13);
  v = vsetq_lane_u8(x[14] / y[14], v, 14);
  v = vsetq_lane_u8(x[15] / y[15], v, 15);
  return (v128) v;
}

v128 hs_quotWord16X8(v128 xx, v128 yy)
{
  uint16_t x[8], y[8];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  uint16x8_t v = vdupq_n_u16(0);
  v = vsetq_lane_u16(x[0] / y[0], v, 0);
  v = vsetq_lane_u16(x[1] / y[1], v, 1);
  v = vsetq_lane_u16(x[2] / y[2], v, 2);
  v = vsetq_lane_u16(x[3] / y[3], v, 3);
  v = vsetq_lane_u16(x[4] / y[4], v, 4);
  v = vsetq_lane_u16(x[5] / y[5], v, 5);
  v = vsetq_lane_u16(x[6] / y[6], v, 6);
  v = vsetq_lane_u16(x[7] / y[7], v, 7);
  return (v128) v;
}

v128 hs_quotWord32X4(v128 xx, v128 yy)
{
  uint32_t x[4], y[4];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  uint32x4_t v = vdupq_n_u32(0);
  v = vsetq_lane_u32(x[0] / y[0], v, 0);
  v = vsetq_lane_u32(x[1] / y[1], v, 1);
  v = vsetq_lane_u32(x[2] / y[2], v, 2);
  v = vsetq_lane_u32(x[3] / y[3], v, 3);
  return (v128) v;
}

v128 hs_quotWord64X2(v128 xx, v128 yy)
{
  uint64_t x[2], y[2];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  uint64x2_t v = vdupq_n_u64(0);
  v = vsetq_lane_u64(x[0] / y[0], v, 0);
  v = vsetq_lane_u64(x[1] / y[1], v, 1);
  return (v128) v;
}

v128 hs_remInt8X16(v128 xx, v128 yy)
{
  int8_t x[16], y[16];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  int8x16_t v = vdupq_n_s8(0);
  v = vsetq_lane_s8(x[0] % y[0], v, 0);
  v = vsetq_lane_s8(x[1] % y[1], v, 1);
  v = vsetq_lane_s8(x[2] % y[2], v, 2);
  v = vsetq_lane_s8(x[3] % y[3], v, 3);
  v = vsetq_lane_s8(x[4] % y[4], v, 4);
  v = vsetq_lane_s8(x[5] % y[5], v, 5);
  v = vsetq_lane_s8(x[6] % y[6], v, 6);
  v = vsetq_lane_s8(x[7] % y[7], v, 7);
  v = vsetq_lane_s8(x[8] % y[8], v, 8);
  v = vsetq_lane_s8(x[9] % y[9], v, 9);
  v = vsetq_lane_s8(x[10] % y[10], v, 10);
  v = vsetq_lane_s8(x[11] % y[11], v, 11);
  v = vsetq_lane_s8(x[12] % y[12], v, 12);
  v = vsetq_lane_s8(x[13] % y[13], v, 13);
  v = vsetq_lane_s8(x[14] % y[14], v, 14);
  v = vsetq_lane_s8(x[15] % y[15], v, 15);
  return (v128) v;
}

v128 hs_remInt16X8(v128 xx, v128 yy)
{
  int16_t x[8], y[8];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  int16x8_t v = vdupq_n_s16(0);
  v = vsetq_lane_s16(x[0] % y[0], v, 0);
  v = vsetq_lane_s16(x[1] % y[1], v, 1);
  v = vsetq_lane_s16(x[2] % y[2], v, 2);
  v = vsetq_lane_s16(x[3] % y[3], v, 3);
  v = vsetq_lane_s16(x[4] % y[4], v, 4);
  v = vsetq_lane_s16(x[5] % y[5], v, 5);
  v = vsetq_lane_s16(x[6] % y[6], v, 6);
  v = vsetq_lane_s16(x[7] % y[7], v, 7);
  return (v128) v;
}

v128 hs_remInt32X4(v128 xx, v128 yy)
{
  int32_t x[4], y[4];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  int32x4_t v = vdupq_n_s32(0);
  v = vsetq_lane_s32(x[0] % y[0], v, 0);
  v = vsetq_lane_s32(x[1] % y[1], v, 1);
  v = vsetq_lane_s32(x[2] % y[2], v, 2);
  v = vsetq_lane_s32(x[3] % y[3], v, 3);
  return (v128) v;
}

v128 hs_remInt64X2(v128 xx, v128 yy)
{
  int64_t x[2], y[2];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  int64x2_t v = vdupq_n_s64(0);
  v = vsetq_lane_s64(x[0] % y[0], v, 0);
  v = vsetq_lane_s64(x[1] % y[1], v, 1);
  return (v128) v;
}

v128 hs_remWord8X16(v128 xx, v128 yy)
{
  uint8_t x[16], y[16];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  uint8x16_t v = vdupq_n_u8(0);
  v = vsetq_lane_u8(x[0] % y[0], v, 0);
  v = vsetq_lane_u8(x[1] % y[1], v, 1);
  v = vsetq_lane_u8(x[2] % y[2], v, 2);
  v = vsetq_lane_u8(x[3] % y[3], v, 3);
  v = vsetq_lane_u8(x[4] % y[4], v, 4);
  v = vsetq_lane_u8(x[5] % y[5], v, 5);
  v = vsetq_lane_u8(x[6] % y[6], v, 6);
  v = vsetq_lane_u8(x[7] % y[7], v, 7);
  v = vsetq_lane_u8(x[8] % y[8], v, 8);
  v = vsetq_lane_u8(x[9] % y[9], v, 9);
  v = vsetq_lane_u8(x[10] % y[10], v, 10);
  v = vsetq_lane_u8(x[11] % y[11], v, 11);
  v = vsetq_lane_u8(x[12] % y[12], v, 12);
  v = vsetq_lane_u8(x[13] % y[13], v, 13);
  v = vsetq_lane_u8(x[14] % y[14], v, 14);
  v = vsetq_lane_u8(x[15] % y[15], v, 15);
  return (v128) v;
}

v128 hs_remWord16X8(v128 xx, v128 yy)
{
  uint16_t x[8], y[8];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  uint16x8_t v = vdupq_n_u16(0);
  v = vsetq_lane_u16(x[0] % y[0], v, 0);
  v = vsetq_lane_u16(x[1] % y[1], v, 1);
  v = vsetq_lane_u16(x[2] % y[2], v, 2);
  v = vsetq_lane_u16(x[3] % y[3], v, 3);
  v = vsetq_lane_u16(x[4] % y[4], v, 4);
  v = vsetq_lane_u16(x[5] % y[5], v, 5);
  v = vsetq_lane_u16(x[6] % y[6], v, 6);
  v = vsetq_lane_u16(x[7] % y[7], v, 7);
  return (v128) v;
}

v128 hs_remWord32X4(v128 xx, v128 yy)
{
  uint32_t x[4], y[4];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  uint32x4_t v = vdupq_n_u32(0);
  v = vsetq_lane_u32(x[0] % y[0], v, 0);
  v = vsetq_lane_u32(x[1] % y[1], v, 1);
  v = vsetq_lane_u32(x[2] % y[2], v, 2);
  v = vsetq_lane_u32(x[3] % y[3], v, 3);
  return (v128) v;
}

v128 hs_remWord64X2(v128 xx, v128 yy)
{
  uint64_t x[2], y[2];
  memcpy(x, &xx, 16);
  memcpy(y, &yy, 16);
  uint64x2_t v = vdupq_n_u64(0);
  v = vsetq_lane_u64(x[0] % y[0], v, 0);
  v = vsetq_lane_u64(x[1] % y[1], v, 1);
  return (v128) v;
}

#endif
