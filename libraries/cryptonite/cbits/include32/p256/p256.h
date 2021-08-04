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

#ifndef SYSTEM_CORE_INCLUDE_MINCRYPT_LITE_P256_H_
#define SYSTEM_CORE_INCLUDE_MINCRYPT_LITE_P256_H_

// Collection of routines manipulating 256 bit unsigned integers.
// Just enough to implement ecdsa-p256 and related algorithms.

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define P256_BITSPERDIGIT 32
#define P256_NDIGITS 8
#define P256_NBYTES 32

// n' such as n * n' = -1 mod (2^32)
#define P256_MONTGOMERY_FACTOR 0xEE00BC4F

#define P256_LITERAL(lo,hi) (lo), (hi)

typedef int cryptonite_p256_err;
typedef uint32_t cryptonite_p256_digit;
typedef int32_t cryptonite_p256_sdigit;
typedef uint64_t cryptonite_p256_ddigit;
typedef int64_t cryptonite_p256_sddigit;

// Defining cryptonite_p256_int as struct to leverage struct assigment.
typedef struct {
  cryptonite_p256_digit a[P256_NDIGITS];
} cryptonite_p256_int;

extern const cryptonite_p256_int cryptonite_SECP256r1_n;  // Curve order
extern const cryptonite_p256_int cryptonite_SECP256r1_p;  // Curve prime
extern const cryptonite_p256_int cryptonite_SECP256r1_b;  // Curve param

// Initialize a cryptonite_p256_int to zero.
void cryptonite_p256_init(cryptonite_p256_int* a);

// Clear a cryptonite_p256_int to zero.
void cryptonite_p256_clear(cryptonite_p256_int* a);

// Return bit. Index 0 is least significant.
int cryptonite_p256_get_bit(const cryptonite_p256_int* a, int index);

// b := a % MOD
void cryptonite_p256_mod(
    const cryptonite_p256_int* MOD,
    const cryptonite_p256_int* a,
    cryptonite_p256_int* b);

// c := a * (top_b | b) % MOD
void cryptonite_p256_modmul(
    const cryptonite_p256_int* MOD,
    const cryptonite_p256_int* a,
    const cryptonite_p256_digit top_b,
    const cryptonite_p256_int* b,
    cryptonite_p256_int* c);

// b := 1 / a % MOD
// MOD best be SECP256r1_n
void cryptonite_p256_modinv(
    const cryptonite_p256_int* MOD,
    const cryptonite_p256_int* a,
    cryptonite_p256_int* b);

// b := 1 / a % MOD
// MOD best be SECP256r1_n
// Faster than cryptonite_p256_modinv()
void cryptonite_p256_modinv_vartime(
    const cryptonite_p256_int* MOD,
    const cryptonite_p256_int* a,
    cryptonite_p256_int* b);

// b := a << (n % P256_BITSPERDIGIT)
// Returns the bits shifted out of most significant digit.
cryptonite_p256_digit cryptonite_p256_shl(const cryptonite_p256_int* a, int n, cryptonite_p256_int* b);

// b := a >> (n % P256_BITSPERDIGIT)
void cryptonite_p256_shr(const cryptonite_p256_int* a, int n, cryptonite_p256_int* b);

int cryptonite_p256_is_zero(const cryptonite_p256_int* a);
int cryptonite_p256_is_odd(const cryptonite_p256_int* a);
int cryptonite_p256_is_even(const cryptonite_p256_int* a);

// Returns -1, 0 or 1.
int cryptonite_p256_cmp(const cryptonite_p256_int* a, const cryptonite_p256_int *b);

// c: = a - b
// Returns -1 on borrow.
int cryptonite_p256_sub(const cryptonite_p256_int* a, const cryptonite_p256_int* b, cryptonite_p256_int* c);

// c := a + b
// Returns 1 on carry.
int cryptonite_p256_add(const cryptonite_p256_int* a, const cryptonite_p256_int* b, cryptonite_p256_int* c);

// c := a + (single digit)b
// Returns carry 1 on carry.
int cryptonite_p256_add_d(const cryptonite_p256_int* a, cryptonite_p256_digit b, cryptonite_p256_int* c);

// ec routines.

// {out_x,out_y} := nG
void cryptonite_p256_base_point_mul(const cryptonite_p256_int *n,
                         cryptonite_p256_int *out_x,
                         cryptonite_p256_int *out_y);

// {out_x,out_y} := n{in_x,in_y}
void cryptonite_p256_point_mul(const cryptonite_p256_int *n,
                    const cryptonite_p256_int *in_x,
                    const cryptonite_p256_int *in_y,
                    cryptonite_p256_int *out_x,
                    cryptonite_p256_int *out_y);

// {out_x,out_y} := n1G + n2{in_x,in_y}
void cryptonite_p256_points_mul_vartime(
    const cryptonite_p256_int *n1, const cryptonite_p256_int *n2,
    const cryptonite_p256_int *in_x, const cryptonite_p256_int *in_y,
    cryptonite_p256_int *out_x, cryptonite_p256_int *out_y);

// Return whether point {x,y} is on curve.
int cryptonite_p256_is_valid_point(const cryptonite_p256_int* x, const cryptonite_p256_int* y);

// Outputs big-endian binary form. No leading zero skips.
void cryptonite_p256_to_bin(const cryptonite_p256_int* src, uint8_t dst[P256_NBYTES]);

// Reads from big-endian binary form,
// thus pre-pad with leading zeros if short.
void cryptonite_p256_from_bin(const uint8_t src[P256_NBYTES], cryptonite_p256_int* dst);

#define P256_DIGITS(x) ((x)->a)
#define P256_DIGIT(x,y) ((x)->a[y])

#define P256_ZERO {{0}}
#define P256_ONE {{1}}

#ifdef __cplusplus
}
#endif

#endif  // SYSTEM_CORE_INCLUDE_MINCRYPT_LITE_P256_H_
