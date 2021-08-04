/*
 * Copyright 2013 The Android Open Source Project
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of Google Inc. nor the names of its contributors may
 *       be used to endorse or promote products derived from this software
 *       without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY Google Inc. ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
 * EVENT SHALL Google Inc. BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

// This is an implementation of the P256 finite field. It's written to be
// portable and still constant-time.
//
// WARNING: Implementing these functions in a constant-time manner is far from
//          obvious. Be careful when touching this code.
//
// See http://www.imperialviolet.org/2010/12/04/ecc.html ([1]) for background.

#include <stdint.h>
#include <stdio.h>

#include <string.h>
#include <stdlib.h>

#include "p256/p256.h"

typedef uint8_t u8;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int64_t s64;
typedef __uint128_t u128;

/* Our field elements are represented as five 64-bit limbs.
 *
 * The value of an felem (field element) is:
 *   x[0] + (x[1] * 2**51) + (x[2] * 2**103) + ... + (x[4] * 2**206)
 *
 * That is, each limb is alternately 51 or 52-bits wide in little-endian
 * order.
 *
 * This means that an felem hits 2**257, rather than 2**256 as we would like.
 *
 * Finally, the values stored in an felem are in Montgomery form. So the value
 * |y| is stored as (y*R) mod p, where p is the P-256 prime and R is 2**257.
 */
typedef u64 limb;
#define NLIMBS 5
typedef limb felem[NLIMBS];

static const limb kBottom51Bits = 0x7ffffffffffff;
static const limb kBottom52Bits = 0xfffffffffffff;

/* kOne is the number 1 as an felem. It's 2**257 mod p split up into 51 and
 * 52-bit words. */
static const felem kOne = {
    2, 0xfc00000000000, 0x7ffffffffffff, 0xfff7fffffffff, 0x7ffff
};
static const felem kZero = {0};
static const felem kP = {
    0x7ffffffffffff, 0x1fffffffffff, 0, 0x4000000000, 0x3fffffffc0000
};
static const felem k2P = {
    0x7fffffffffffe, 0x3fffffffffff, 0, 0x8000000000, 0x7fffffff80000
};
/* kPrecomputed contains precomputed values to aid the calculation of scalar
 * multiples of the base point, G. It's actually two, equal length, tables
 * concatenated.
 *
 * The first table contains (x,y) felem pairs for 16 multiples of the base
 * point, G.
 *
 *   Index  |  Index (binary) | Value
 *       0  |           0000  | 0G (all zeros, omitted)
 *       1  |           0001  | G
 *       2  |           0010  | 2**64G
 *       3  |           0011  | 2**64G + G
 *       4  |           0100  | 2**128G
 *       5  |           0101  | 2**128G + G
 *       6  |           0110  | 2**128G + 2**64G
 *       7  |           0111  | 2**128G + 2**64G + G
 *       8  |           1000  | 2**192G
 *       9  |           1001  | 2**192G + G
 *      10  |           1010  | 2**192G + 2**64G
 *      11  |           1011  | 2**192G + 2**64G + G
 *      12  |           1100  | 2**192G + 2**128G
 *      13  |           1101  | 2**192G + 2**128G + G
 *      14  |           1110  | 2**192G + 2**128G + 2**64G
 *      15  |           1111  | 2**192G + 2**128G + 2**64G + G
 *
 * The second table follows the same style, but the terms are 2**32G,
 * 2**96G, 2**160G, 2**224G.
 *
 * This is ~2KB of data. */
static const limb kPrecomputed[NLIMBS * 2 * 15 * 2] = {
    0x661a831522878, 0xf17fb6d805e79, 0x5889441d6ea57, 0xae33cfdb995bb, 0xc482fbb529ba,
    0x4a6af9d2aac15, 0x90e867917377c, 0x487cc962d2ae3, 0xec2a97443446e, 0x2b8ff8c52c42,
    0x45f8a2d41a576, 0xb06988d2653e4, 0x718b22c357305, 0x33fc920e79d2b, 0x17af34b0fe8db,
    0x38e17eb402f2f, 0x3382558649705, 0x47f6d48f482d1, 0x7bd42488d9b83, 0x3b247c8b86b78,
    0x4d08fc26f7778, 0x7a29a82fb2795, 0x75cd18f90d11a, 0xad8e213b0bc, 0x2d5f0142899e8,
    0x506f98098fb57, 0x2f0c98301e4aa, 0x39b30dd5cf67d, 0x9c146498ab13c, 0xa5db92df5b7b,
    0x184897fc4124a, 0xe3f73a19d8aa, 0x4e1c18e47066b, 0x27b2d4b52eaee, 0x30eac3ea10e99,
    0x4e74546e2e7d5, 0x1f4dde2d97a1d, 0x6ead0f88e1200, 0x7dec87c220f02, 0x3d08ff096310f,
    0x23e5659633ffa, 0x6ec648f08c722, 0x3172a3806ea35, 0xf6e5b681eb3c5, 0x2c3758260f89d,
    0x38dca4fd1da12, 0xf06067b78830d, 0x3194be87a068c, 0x78893c7eb602b, 0xcead60438432,
    0x6ee69a56a67ab, 0xd886f77701895, 0x67b0a4d9cee2b, 0x3586bbf3e4d53, 0x1db6f32921d93,
    0x260756ca4b366, 0x4f40e9d2039fa, 0x4f3f09f5a82bf, 0xccde2d641e8cd, 0x305a30cd2e8c5,
    0x471c235cb5439, 0xab279cd962f5a, 0x17e1fb6e2dd94, 0xfe64589800a77, 0xe8793d99775f,
    0x48c62f4e614aa, 0xbf76ef20eb2a4, 0x669c672556c, 0x24683e0eff056, 0x12252b369ab76,
    0x821de9f162d5, 0xf911ec99a95be, 0x6721f065c906b, 0x58d452035c736, 0x1f9f01a6a15,
    0x6135009b7d8d3, 0xdaeeeb417dfc0, 0x63865fea0ee17, 0x6e0a304b939d6, 0x204ba2076833d,
    0x4ade586f35669, 0x2c1077e34611a, 0x5b1a3bea3b81a, 0xf97d018a22c8b, 0x38d7996b08af8,
    0x6ea62baeb7aa0, 0xebdcbd9ef2670, 0x35dc8fe0df3fe, 0xe458309d20c24, 0x11e87898716a0,
    0x7c44bab7cb456, 0xd64d3cf1bb64, 0x189bff1bf9e66, 0xb5218a049311, 0x285dda6cbcc81,
    0x3238dcafd8c7c, 0x607736c8de0, 0xdb83d99508b1, 0x4e1a0d404cd81, 0x1588008c00ff2,
    0x16b8b36722b27, 0x876609c3f3f1a, 0x66b72ef0e17d6, 0x705f8a279d568, 0x2eaac4cd01fdd,
    0x1171ce9705fe9, 0xffc79cd3264ee, 0x700c8ab4b80f0, 0x208d3d4f57a1, 0x337262a8ca4eb,
    0x297fd01d843fd, 0xa90956fa097f8, 0x529759fdb3845, 0x1d78c5e2d0397, 0x3d6938a4adbf3,
    0x16d5853560b66, 0xf138946b9a430, 0x2ab79f4dea6a0, 0xd42053ee43ae1, 0x3b9c3ef1cf870,
    0x598934ad81baf, 0x5f1821b1d07a7, 0x416bb3a973ff3, 0x23f07bd0a047a, 0x19bdc2e09f786,
    0x56dc9981cd51f, 0xfbace23c8cd65, 0x673bd3bf5b52e, 0x46a95d229fd61, 0xe09ad64bcfb1,
    0xe5292b91f17d, 0xfeefcd8afc287, 0x58f52b0a58711, 0x4800f20c201ef, 0x2084fce608f67,
    0x12ba0b128ae0b, 0x5977ae17030b4, 0x101126ee420f6, 0xf70823495c6bd, 0xde19a27d7770,
    0x5c6ac852260e8, 0x9d22950ac4356, 0x441cca955246c, 0x660a34e5332d9, 0x14ac8ea92f8d2,
    0x6b6d7709f307e, 0x67d7e13879db, 0x2ea8626f9fbbd, 0x99609006a4b40, 0x31bb2a8f8c779,
    0x10c04828ea335, 0xae9acdcbc080a, 0x617af2342607a, 0xc7494ea53e553, 0x2ca9e2872defa,
    0x6c399fab21f1f, 0xab139b245e758, 0x3ad933dcba589, 0x4797fecb08811, 0x31f5dbf8f594,
    0x7dc6361cc7a69, 0xc8a7953ead3f9, 0x79ed693d18015, 0x418a024999a6a, 0x2c4fdc9436aa,
    0x1eb98cb06aa75, 0x2989592796a9c, 0x11194821e425, 0xe27a648228388, 0x35d834b6c12a0,
    0x541807713b532, 0x7ae0a1008aaee, 0x7017a29bcb5e, 0x6b193c23c315c, 0x19bd25ac82f2a,
    0x6a01a43eef294, 0xddf5b5fd84f19, 0x33f5ba081c016, 0xdeb052d1bc082, 0x6b2f06afa617,
    0x7ca1eda6a939f, 0xbdeb35997b50c, 0x47f2d1bccda5, 0xc2ff4adfed667, 0x87712997be4,
    0x21fc2e2b37659, 0xf7d62cd5ed951, 0x27fa9cbdf7efa, 0xba25582bf3a6b, 0x2a42b8bd89398,
    0x6d377d07eecd2, 0x9ca1df5af387, 0x1109e3427e2ba, 0xce4aa4572a19, 0x103baaef71e16,
    0x2c3b2dfde328a, 0xbec4b4a30e1ef, 0x37d92a86204f3, 0x806cfde68eb39, 0x246e2f72b8aa5,
    0x68d3de93462a9, 0x53b8acba6bbc3, 0x2492a70fa1696, 0x38c62d5760f55, 0x15096fe4904f2,
    0x4e44e9bed3e3a, 0xb28bfd79cc9bc, 0x6a77513839320, 0x480dcec6739db, 0x3601b739f2465,
    0x43c348e2a7e1, 0xe448106327879, 0x175d9cae1b0ed, 0xd3b89dee743b8, 0x392d73ca255bc,
    0x32946db0d3a18, 0x9261b09907cc, 0x5ba517a755722, 0x51f24fdaf5184, 0x1cdc732989ed8,
    0x2f7806ba16694, 0xae0c9f029f8d0, 0xd8b45102ce1, 0xca1c7db9316d6, 0x162088a67066f,
    0x39de35b2b4162, 0xa19f550d88ae9, 0x7921b27026cde, 0x94b936b66e900, 0x1023bd5fa17fc,
    0x436837814cfa4, 0x29113492283c4, 0x66d1cdd8b51d8, 0xa540702278eb2, 0x47ef1b29285d,
    0x587b50917e50e, 0xb4cda75bab3b, 0x112520b0a9886, 0x66b9ac16fee49, 0x17bf17e92b2eb,
    0x2456a2f150ed7, 0xfa214412d0280, 0x3ca7dd947fe5b, 0xa72c28598d58a, 0x255d945efc3e,
    0x2873f04e0f215, 0x74178fd1af57b, 0x788848b5b2d6, 0xb1ffafaae0db6, 0x32a1b7b3cbb2a,
    0x4bd9935d6b2da, 0x9c08f24ad30a5, 0x4e58407a80f, 0x1b3a3825a5b17, 0x6547e9fc82f5,
    0x47484aa3656c3, 0x6ee43f341a494, 0x64a98f87adea2, 0x619b3f8e95f01, 0xb6e513266ed8,
    0x421c2a673090, 0xa1c1de32348c7, 0x55b85c3a1e8a3, 0xe05ce8ef330b4, 0x2561e49c15d84,
    0x40aa2d33130fa, 0x12b827d35866f, 0xfe4cf62c8ddb, 0x2fa0ef05bb28d, 0x1c06ca63f1cb8,
    0x32a971863863b, 0xff6fc86830da1, 0x71e7b25a14cf3, 0xea9c5ebb1373a, 0x250bbaa3e1634,
    0x5b5ffeda5b765, 0xf25d2a746331b, 0x115e3a3f43632, 0x67303af43c9d5, 0x14bb538a0e559,
    0x75623687d43b7, 0xa349674a4b38d, 0x613c61829ffc6, 0x689828d8110c7, 0x139115f5af7d5,
    0xf1d856152289, 0x45cbe967168ab, 0x51f38e1680901, 0x34808e8f652b0, 0x1f4a6a921e156,
    0x35dfaf3d8341f, 0xf53ace725cb63, 0x3d86a54eef35b, 0xa103aabaffe2c, 0x2decc36296fbd,
    0x510282be73d6f, 0xd4e6365db206a, 0x4bdc5f5bb8bf3, 0xde7ea32a3aee7, 0x71269e274305,
};


/* Field element operations: */

/* NON_ZERO_TO_ALL_ONES returns:
 *   0xffffffffffffffff for 0 < x <= 2**63
 *   0 for x == 0 or x > 2**63.
 *
 * x must be a u64 or an equivalent type such as limb. */
#define NON_ZERO_TO_ALL_ONES(x) ((((u64)(x) - 1) >> 63) - 1)

/* felem_reduce_carry adds a multiple of p in order to cancel |carry|,
 * which is a term at 2**257.
 *
 * On entry: carry < 2**6, inout[0,2,...] < 2**51, inout[1,3,...] < 2**52.
 * On exit: inout[0,2,..] < 2**52, inout[1,3,...] < 2**53. */
static void felem_reduce_carry(felem inout, limb carry) {
  const u64 carry_mask = NON_ZERO_TO_ALL_ONES(carry);

  inout[0] += carry << 1;
  inout[1] += 0x10000000000000 & carry_mask;
  /* carry < 2**6 thus (carry << 46) < 2**52 and we added 2**52 in the
   * previous line therefore this doesn't underflow. */
  inout[1] -= carry << 46;
  inout[2] += (0x8000000000000 - 1) & carry_mask;
  inout[3] += (0x10000000000000 - 1) & carry_mask;
  inout[3] -= carry << 39;
  /* This may underflow if carry is non-zero but, if so, we'll fix it in the
   * next line. */
  inout[4] -= 1 & carry_mask;
  inout[4] += carry << 19;
}

/* felem_sum sets out = in+in2.
 *
 * On entry, in[i]+in2[i] must not overflow a 64-bit word.
 * On exit: out[0,2,...] < 2**52, out[1,3,...] < 2**53 */
static void felem_sum(felem out, const felem in, const felem in2) {
  limb carry = 0;
  unsigned i;

  for (i = 0;; i++) {
    out[i] = in[i] + in2[i];
    out[i] += carry;
    carry = out[i] >> 51;
    out[i] &= kBottom51Bits;

    i++;
    if (i == NLIMBS)
      break;

    out[i] = in[i] + in2[i];
    out[i] += carry;
    carry = out[i] >> 52;
    out[i] &= kBottom52Bits;
  }

  felem_reduce_carry(out, carry);
}

#define two53m3 (((limb)1) << 53) - (((limb)1) << 3)
#define two54m52p48m2 (((limb)1) << 54) - (((limb)1) << 52) + (((limb)1) << 48) - (((limb)1) << 2)
#define two53m2p0 (((limb)1) << 53) - (((limb)1) << 2) + (((limb)1) << 0)
#define two54m52p41m2 (((limb)1) << 54) - (((limb)1) << 52) + (((limb)1) << 41) - (((limb)1) << 2)
#define two53m21m2p0 (((limb)1) << 53) - (((limb)1) << 21) - (((limb)1) << 2) + (((limb)1) << 0)

/* zero53 is 0 mod p. */
static const felem zero53 = { two53m3, two54m52p48m2, two53m2p0, two54m52p41m2, two53m21m2p0 };

/* felem_diff sets out = in-in2.
 *
 * On entry: in[0,2,...] < 2**52, in[1,3,...] < 2**53 and
 *           in2[0,2,...] < 2**52, in2[1,3,...] < 2**53.
 * On exit: out[0,2,...] < 2**52, out[1,3,...] < 2**53. */
static void felem_diff(felem out, const felem in, const felem in2) {
  limb carry = 0;
  unsigned i;

   for (i = 0;; i++) {
    out[i] = in[i] - in2[i];
    out[i] += zero53[i];
    out[i] += carry;
    carry = out[i] >> 51;
    out[i] &= kBottom51Bits;

    i++;
    if (i == NLIMBS)
      break;

    out[i] = in[i] - in2[i];
    out[i] += zero53[i];
    out[i] += carry;
    carry = out[i] >> 52;
    out[i] &= kBottom52Bits;
  }

  felem_reduce_carry(out, carry);
}

/* felem_reduce_degree sets out = tmp/R mod p where tmp contains 64-bit words
 * with the same 51,52,... bit positions as an felem.
 *
 * The values in felems are in Montgomery form: x*R mod p where R = 2**257.
 * Since we just multiplied two Montgomery values together, the result is
 * x*y*R*R mod p. We wish to divide by R in order for the result also to be
 * in Montgomery form.
 *
 * On entry: tmp[i] < 2**128
 * On exit: out[0,2,...] < 2**52, out[1,3,...] < 2**53 */
static void felem_reduce_degree(felem out, u128 tmp[9]) {
   /* The following table may be helpful when reading this code:
    *
    * Limb number:   0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10
    * Width (bits):  51| 52| 51| 52| 51| 52| 51| 52| 51| 52| 51
    * Start bit:     0 | 51|103|154|206|257|309|360|412|463|515
    *   (odd phase): 0 | 52|103|155|206|258|309|361|412|464|515 */
  limb tmp2[10], carry, x, xShiftedMask;
  unsigned i;

  /* tmp contains 128-bit words with the same 51,52,51-bit positions as an
   * felem. So the top of an element of tmp might overlap with another
   * element two positions down. The following loop eliminates this
   * overlap. */
  tmp2[0] = (limb)(tmp[0] & kBottom51Bits);

  /* In the following we use "(limb) tmp[x]" and "(limb) (tmp[x]>>64)" to try
   * and hint to the compiler that it can do a single-word shift by selecting
   * the right register rather than doing a double-word shift and truncating
   * afterwards. */
  tmp2[1] = ((limb) tmp[0]) >> 51;
  tmp2[1] |= (((limb)(tmp[0] >> 64)) << 13) & kBottom52Bits;
  tmp2[1] += ((limb) tmp[1]) & kBottom52Bits;
  carry = tmp2[1] >> 52;
  tmp2[1] &= kBottom52Bits;

  for (i = 2; i < 9; i++) {
    tmp2[i] = ((limb)(tmp[i - 2] >> 64)) >> 39;
    tmp2[i] += ((limb)(tmp[i - 1])) >> 52;
    tmp2[i] += (((limb)(tmp[i - 1] >> 64)) << 12) & kBottom51Bits;
    tmp2[i] += ((limb) tmp[i]) & kBottom51Bits;
    tmp2[i] += carry;
    carry = tmp2[i] >> 51;
    tmp2[i] &= kBottom51Bits;

    i++;
    if (i == 9)
      break;
    tmp2[i] = ((limb)(tmp[i - 2] >> 64)) >> 39;
    tmp2[i] += ((limb)(tmp[i - 1])) >> 51;
    tmp2[i] += (((limb)(tmp[i - 1] >> 64)) << 13) & kBottom52Bits;
    tmp2[i] += ((limb) tmp[i]) & kBottom52Bits;
    tmp2[i] += carry;
    carry = tmp2[i] >> 52;
    tmp2[i] &= kBottom52Bits;
  }

  tmp2[9] = ((limb)(tmp[7] >> 64)) >> 39;
  tmp2[9] += ((limb)(tmp[8])) >> 51;
  tmp2[9] += (((limb)(tmp[8] >> 64)) << 13);
  tmp2[9] += carry;

  /* Montgomery elimination of terms.
   *
   * Since R is 2**257, we can divide by R with a bitwise shift if we can
   * ensure that the right-most 257 bits are all zero. We can make that true by
   * adding multiplies of p without affecting the value.
   *
   * So we eliminate limbs from right to left. Since the bottom 51 bits of p
   * are all ones, then by adding tmp2[0]*p to tmp2 we'll make tmp2[0] == 0.
   * We can do that for 8 further limbs and then right shift to eliminate the
   * extra factor of R. */
  for (i = 0;; i += 2) {
    tmp2[i + 1] += tmp2[i] >> 51;
    x = tmp2[i] & kBottom51Bits;
    xShiftedMask = NON_ZERO_TO_ALL_ONES(x >> 1);
    tmp2[i] = 0;

    /* The bounds calculations for this loop are tricky. Each iteration of
     * the loop eliminates two words by adding values to words to their
     * right.
     *
     * The following table contains the amounts added to each word (as an
     * offset from the value of i at the top of the loop). The amounts are
     * accounted for from the first and second half of the loop separately
     * and are written as, for example, 51 to mean a value <2**51.
     *
     * Word:                   1   2   3   4   5   6
     * Added in top half:     52  44  52  37  50
     *                                    51
     *                                    51
     * Added in bottom half:      51  45  51  38  50
     *                                        52
     *                                        52
     *
     * The value that is currently offset 5 will be offset 3 for the next
     * iteration and then offset 1 for the iteration after that. Therefore
     * the total value added will be the values added at 5, 3 and 1.
     *
     * The following table accumulates these values. The sums at the bottom
     * are written as, for example, 53+45, to mean a value < 2**53+2**45.
     *
     * Word:                   1   2   3   4   5   6   7   8   9
     *                        52  44  52  37  50  50  50  50  50
     *                            51  45  51  38  37  38  37
     *                                52  51  52  51  52  51
     *                                    51  52  51  52  51
     *                                    44  52  51  52
     *                                    51  45  44
     *                                        52
     *                        ------------------------------------
     *                                53+ 53+ 54+ 52+ 53+ 52+
     *                                45  44+ 50+ 51+ 52+ 50+
     *                                    37  45+ 50+ 50+ 37
     *                                        38  44+ 38
     *                                            37
     *
     * So the greatest amount is added to tmp2[5]. If tmp2[5] has an initial
     * value of <2**52, then the maximum value will be < 2**54 + 2**52 + 2**50 +
     * 2**45 + 2**38, which is < 2**64, as required. */
    tmp2[i + 1] += (x << 45) & kBottom52Bits;
    tmp2[i + 2] += x >> 7;

    tmp2[i + 3] += (x << 38) & kBottom52Bits;
    tmp2[i + 4] += x >> 14;

    /* On tmp2[i + 4], when x < 2**1, the subtraction with (x << 18) will not
     * underflow because it is balanced with the (x << 50) term.  On the next
     * word tmp2[i + 5], terms with (x >> 1) and (x >> 33) are both zero and
     * there is no underflow either.
     *
     * When x >= 2**1, we add 2**51 to tmp2[i + 4] to avoid an underflow.
     * Removing 1 from tmp2[i + 5] is safe because (x >> 1) - (x >> 33) is
     * strictly positive.
     */
    tmp2[i + 4] += 0x8000000000000 & xShiftedMask;
    tmp2[i + 5] -= 1 & xShiftedMask;

    tmp2[i + 4] -= (x << 18) & kBottom51Bits;
    tmp2[i + 4] += (x << 50) & kBottom51Bits;
    tmp2[i + 5] += (x >> 1) - (x >> 33);

    if (i+1 == NLIMBS)
      break;
    tmp2[i + 2] += tmp2[i + 1] >> 52;
    x = tmp2[i + 1] & kBottom52Bits;
    xShiftedMask = NON_ZERO_TO_ALL_ONES(x >> 2);
    tmp2[i + 1] = 0;

    tmp2[i + 2] += (x << 44) & kBottom51Bits;
    tmp2[i + 3] += x >> 7;

    tmp2[i + 4] += (x << 37) & kBottom51Bits;
    tmp2[i + 5] += x >> 14;

    /* On tmp2[i + 5], when x < 2**2, the subtraction with (x << 18) will not
     * underflow because it is balanced with the (x << 50) term.  On the next
     * word tmp2[i + 6], terms with (x >> 2) and (x >> 34) are both zero and
     * there is no underflow either.
     *
     * When x >= 2**2, we add 2**52 to tmp2[i + 5] to avoid an underflow.
     * Removing 1 from tmp2[i + 6] is safe because (x >> 2) - (x >> 34) is
     * stricly positive.
     */
    tmp2[i + 5] += 0x10000000000000 & xShiftedMask;
    tmp2[i + 6] -= 1 & xShiftedMask;

    tmp2[i + 5] -= (x << 18) & kBottom52Bits;
    tmp2[i + 5] += (x << 50) & kBottom52Bits;
    tmp2[i + 6] += (x >> 2) - (x >> 34);
  }

  /* We merge the right shift with a carry chain. The words above 2**257 have
   * widths of 52,51,... which we need to correct when copying them down.  */
  carry = 0;
  for (i = 0; i < 4; i++) {
    out[i] = tmp2[i + 5];
    out[i] += carry;
    carry = out[i] >> 51;
    out[i] &= kBottom51Bits;

    i++;
    out[i] = tmp2[i + 5] << 1;
    out[i] += carry;
    carry = out[i] >> 52;
    out[i] &= kBottom52Bits;
  }

  out[4] = tmp2[9];
  out[4] += carry;
  carry = out[4] >> 51;
  out[4] &= kBottom51Bits;

  felem_reduce_carry(out, carry);
}

/* felem_square sets out=in*in.
 *
 * On entry: in[0,2,...] < 2**52, in[1,3,...] < 2**53.
 * On exit: out[0,2,...] < 2**52, out[1,3,...] < 2**53. */
static void felem_square(felem out, const felem in) {
  u128 tmp[9], x1x1, x3x3;

  x1x1 = ((u128) in[1]) * in[1];
  x3x3 = ((u128) in[3]) * in[3];

  tmp[0] = ((u128) in[0]) * (in[0] << 0);
  tmp[1] = ((u128) in[0]) * (in[1] << 1) + ((x1x1 & 1) << 51);
  tmp[2] = ((u128) in[0]) * (in[2] << 1) + (x1x1 >> 1);
  tmp[3] = ((u128) in[0]) * (in[3] << 1) +
           ((u128) in[1]) * (in[2] << 1);
  tmp[4] = ((u128) in[0]) * (in[4] << 1) +
           ((u128) in[1]) * (in[3] << 0) +
           ((u128) in[2]) * (in[2] << 0);
  tmp[5] = ((u128) in[1]) * (in[4] << 1) +
           ((u128) in[2]) * (in[3] << 1) + ((x3x3 & 1) << 51);
  tmp[6] = ((u128) in[2]) * (in[4] << 1) + (x3x3 >> 1);
  tmp[7] = ((u128) in[3]) * (in[4] << 1);
  tmp[8] = ((u128) in[4]) * (in[4] << 0);

  felem_reduce_degree(out, tmp);
}

/* felem_mul sets out=in*in2.
 *
 * On entry: in[0,2,...] < 2**52, in[1,3,...] < 2**53 and
 *           in2[0,2,...] < 2**52, in2[1,3,...] < 2**53.
 * On exit: out[0,2,...] < 2**52, out[1,3,...] < 2**53. */
static void felem_mul(felem out, const felem in, const felem in2) {
  u128 tmp[9], x1y1, x1y3, x3y1, x3y3;

  x1y1 = ((u128) in[1]) * in2[1];
  x1y3 = ((u128) in[1]) * in2[3];
  x3y1 = ((u128) in[3]) * in2[1];
  x3y3 = ((u128) in[3]) * in2[3];

  tmp[0] = ((u128) in[0]) * in2[0];
  tmp[1] = ((u128) in[0]) * in2[1] +
           ((u128) in[1]) * in2[0] + ((x1y1 & 1) << 51);
  tmp[2] = ((u128) in[0]) * in2[2] + (x1y1 >> 1) +
           ((u128) in[2]) * in2[0];
  tmp[3] = ((u128) in[0]) * in2[3] +
           ((u128) in[1]) * in2[2] +
           ((u128) in[2]) * in2[1] + ((x1y3 & 1) << 51) +
           ((u128) in[3]) * in2[0] + ((x3y1 & 1) << 51);
  tmp[4] = ((u128) in[0]) * in2[4] + (x1y3 >> 1) +
           ((u128) in[2]) * in2[2] + (x3y1 >> 1) +
           ((u128) in[4]) * in2[0];
  tmp[5] = ((u128) in[1]) * in2[4] +
           ((u128) in[2]) * in2[3] +
           ((u128) in[3]) * in2[2] +
           ((u128) in[4]) * in2[1] + ((x3y3 & 1) << 51);
  tmp[6] = ((u128) in[2]) * in2[4] + (x3y3 >> 1) +
           ((u128) in[4]) * in2[2];
  tmp[7] = ((u128) in[3]) * in2[4] +
           ((u128) in[4]) * in2[3];
  tmp[8] = ((u128) in[4]) * in2[4];

  felem_reduce_degree(out, tmp);
}

static void felem_assign(felem out, const felem in) {
  memcpy(out, in, sizeof(felem));
}

/* felem_scalar_3 sets out=3*out.
 *
 * On entry: out[0,2,...] < 2**52, out[1,3,...] < 2**53.
 * On exit: out[0,2,...] < 2**52, out[1,3,...] < 2**53. */
static void felem_scalar_3(felem out) {
  limb carry = 0;
  unsigned i;

  for (i = 0;; i++) {
    out[i] *= 3;
    out[i] += carry;
    carry = out[i] >> 51;
    out[i] &= kBottom51Bits;

    i++;
    if (i == NLIMBS)
      break;

    out[i] *= 3;
    out[i] += carry;
    carry = out[i] >> 52;
    out[i] &= kBottom52Bits;
  }

  felem_reduce_carry(out, carry);
}

/* felem_scalar_4 sets out=4*out.
 *
 * On entry: out[0,2,...] < 2**52, out[1,3,...] < 2**53.
 * On exit: out[0,2,...] < 2**52, out[1,3,...] < 2**53. */
static void felem_scalar_4(felem out) {
  limb carry = 0, next_carry;
  unsigned i;

  for (i = 0;; i++) {
    next_carry = out[i] >> 49;
    out[i] <<= 2;
    out[i] &= kBottom51Bits;
    out[i] += carry;
    carry = next_carry + (out[i] >> 51);
    out[i] &= kBottom51Bits;

    i++;
    if (i == NLIMBS)
      break;

    next_carry = out[i] >> 50;
    out[i] <<= 2;
    out[i] &= kBottom52Bits;
    out[i] += carry;
    carry = next_carry + (out[i] >> 52);
    out[i] &= kBottom52Bits;
  }

  felem_reduce_carry(out, carry);
}

/* felem_scalar_8 sets out=8*out.
 *
 * On entry: out[0,2,...] < 2**52, out[1,3,...] < 2**53.
 * On exit: out[0,2,...] < 2**52, out[1,3,...] < 2**53. */
static void felem_scalar_8(felem out) {
  limb carry = 0, next_carry;
  unsigned i;

  for (i = 0;; i++) {
    next_carry = out[i] >> 48;
    out[i] <<= 3;
    out[i] &= kBottom51Bits;
    out[i] += carry;
    carry = next_carry + (out[i] >> 51);
    out[i] &= kBottom51Bits;

    i++;
    if (i == NLIMBS)
      break;

    next_carry = out[i] >> 49;
    out[i] <<= 3;
    out[i] &= kBottom52Bits;
    out[i] += carry;
    carry = next_carry + (out[i] >> 52);
    out[i] &= kBottom52Bits;
  }

  felem_reduce_carry(out, carry);
}

/* felem_is_zero_vartime returns 1 iff |in| == 0. It takes a variable amount of
 * time depending on the value of |in|. */
static char felem_is_zero_vartime(const felem in) {
  limb carry;
  int i;
  limb tmp[NLIMBS];

  felem_assign(tmp, in);

  /* First, reduce tmp to a minimal form. */
  do {
    carry = 0;
    for (i = 0;; i++) {
      tmp[i] += carry;
      carry = tmp[i] >> 51;
      tmp[i] &= kBottom51Bits;

      i++;
      if (i == NLIMBS)
        break;

      tmp[i] += carry;
      carry = tmp[i] >> 52;
      tmp[i] &= kBottom52Bits;
    }

    felem_reduce_carry(tmp, carry);
  } while (carry);

  /* tmp < 2**257, so the only possible zero values are 0, p and 2p. */
  return memcmp(tmp, kZero, sizeof(tmp)) == 0 ||
         memcmp(tmp, kP, sizeof(tmp)) == 0 ||
         memcmp(tmp, k2P, sizeof(tmp)) == 0;
}


/* Montgomery operations: */

#define kRDigits {2, 0xfffffffe00000000, 0xffffffffffffffff, 0x1fffffffd} // 2^257 mod p256.p

#define kRInvDigits {0x180000000, 0xffffffff, 0xfffffffe80000001, 0x7fffffff00000001}  // 1 / 2^257 mod p256.p

static const cryptonite_p256_int kR = { kRDigits };
static const cryptonite_p256_int kRInv = { kRInvDigits };

/* to_montgomery sets out = R*in. */
static void to_montgomery(felem out, const cryptonite_p256_int* in) {
  cryptonite_p256_int in_shifted;
  int i;

  cryptonite_p256_init(&in_shifted);
  cryptonite_p256_modmul(&cryptonite_SECP256r1_p, in, 0, &kR, &in_shifted);

  for (i = 0; i < NLIMBS; i++) {
    if ((i & 1) == 0) {
      out[i] = P256_DIGIT(&in_shifted, 0) & kBottom51Bits;
      cryptonite_p256_shr(&in_shifted, 51, &in_shifted);
    } else {
      out[i] = P256_DIGIT(&in_shifted, 0) & kBottom52Bits;
      cryptonite_p256_shr(&in_shifted, 52, &in_shifted);
    }
  }

  cryptonite_p256_clear(&in_shifted);
}

/* from_montgomery sets out=in/R. */
static void from_montgomery(cryptonite_p256_int* out, const felem in) {
  cryptonite_p256_int result, tmp;
  int i, top;

  cryptonite_p256_init(&result);
  cryptonite_p256_init(&tmp);

  cryptonite_p256_add_d(&tmp, in[NLIMBS - 1], &result);
  for (i = NLIMBS - 2; i >= 0; i--) {
    if ((i & 1) == 0) {
      top = cryptonite_p256_shl(&result, 51, &tmp);
    } else {
      top = cryptonite_p256_shl(&result, 52, &tmp);
    }
    top += cryptonite_p256_add_d(&tmp, in[i], &result);
  }

  cryptonite_p256_modmul(&cryptonite_SECP256r1_p, &kRInv, top, &result, out);

  cryptonite_p256_clear(&result);
  cryptonite_p256_clear(&tmp);
}
