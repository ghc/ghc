#define _ISOC99_SOURCE

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <math.h>
#include <float.h>
#include <stdio.h>

#include <gmp.h>

#include "HsFFI.h"
#include "MachDeps.h"

// GMP 4.x compatibility
#if !defined(__GNU_MP_VERSION)
# error __GNU_MP_VERSION not defined
#elif __GNU_MP_VERSION < 4
# error need GMP 4.0 or later
#elif __GNU_MP_VERSION < 5
typedef unsigned long int mp_bitcnt_t;
#endif

#if (GMP_NUMB_BITS) != (GMP_LIMB_BITS)
# error GMP_NUMB_BITS != GMP_LIMB_BITS not supported
#endif

#if (WORD_SIZE_IN_BITS) != (GMP_LIMB_BITS)
# error WORD_SIZE_IN_BITS != GMP_LIMB_BITS not supported
#endif

// sanity check
#if (SIZEOF_HSWORD*8) != WORD_SIZE_IN_BITS
# error (SIZEOF_HSWORD*8) != WORD_SIZE_IN_BITS
#endif

/* Perform arithmetic right shift on MPNs (multi-precision naturals)
 *
 * pre-conditions:
 *  - 0 < count < sn*GMP_NUMB_BITS
 *  - rn = sn - floor(count / GMP_NUMB_BITS)
 *  - sn > 0
 *
 * write {sp,sn} right-shifted by count bits into {rp,rn}
 *
 * return value: most-significant limb stored in {rp,rn} result
 */
mp_limb_t
integer_gmp_mpn_rshift (mp_limb_t rp[], const mp_limb_t sp[], mp_size_t sn,
                        mp_bitcnt_t count)
{
  const mp_size_t    limb_shift = count / GMP_NUMB_BITS;
  const unsigned int bit_shift  = count % GMP_NUMB_BITS;
  const mp_size_t    rn         = sn - limb_shift;

  if (bit_shift)
    mpn_rshift(rp, &sp[limb_shift], rn, bit_shift);
  else
    memcpy(rp, &sp[limb_shift], rn*sizeof(mp_limb_t));

  return rp[rn-1];
}

/* Twos-complement version of 'integer_gmp_mpn_rshift' for performing
 * arithmetic right shifts on "negative" MPNs.
 *
 * Same pre-conditions as 'integer_gmp_mpn_rshift'
 *
 * This variant is needed to operate on MPNs interpreted as negative
 * numbers, which require "rounding" towards minus infinity iff a
 * non-zero bit is shifted out.
 */
mp_limb_t
integer_gmp_mpn_rshift_2c (mp_limb_t rp[], const mp_limb_t sp[],
                           const mp_size_t sn, const mp_bitcnt_t count)
{
  const mp_size_t    limb_shift = count / GMP_NUMB_BITS;
  const unsigned int bit_shift  = count % GMP_NUMB_BITS;
  const mp_size_t    rn         = sn - limb_shift;

  // whether non-zero bits were shifted out
  bool nz_shift_out = false;

  if (bit_shift) {
    if (mpn_rshift(rp, &sp[limb_shift], rn, bit_shift))
      nz_shift_out = true;
  } else
    memcpy(rp, &sp[limb_shift], rn*sizeof(mp_limb_t));

  if (!nz_shift_out)
    for (unsigned i = 0; i < limb_shift; i++)
      if (sp[i]) {
        nz_shift_out = true;
        break;
      }

  // round if non-zero bits were shifted out
  if (nz_shift_out)
    if (mpn_add_1(rp, rp, rn, 1))
      abort(); /* should never happen */

  return rp[rn-1];
}

/* Perform left-shift operation on MPN
 *
 * pre-conditions:
 *  - 0 < count
 *  - rn = sn + ceil(count / GMP_NUMB_BITS)
 *  - sn > 0
 *
 * return value: most-significant limb stored in {rp,rn} result
 */
mp_limb_t
integer_gmp_mpn_lshift (mp_limb_t rp[], const mp_limb_t sp[],
                        const mp_size_t sn, const mp_bitcnt_t count)
{
  const mp_size_t    limb_shift = count / GMP_NUMB_BITS;
  const unsigned int bit_shift  = count % GMP_NUMB_BITS;
  const mp_size_t    rn0        = sn + limb_shift;

  memset(rp, 0, limb_shift*sizeof(mp_limb_t));
  if (bit_shift) {
    const mp_limb_t msl = mpn_lshift(&rp[limb_shift], sp, sn, bit_shift);
    rp[rn0] = msl;
    return msl;
  } else {
    memcpy(&rp[limb_shift], sp, sn*sizeof(mp_limb_t));
    return rp[rn0-1];
  }
}

/*
 *
 * sign of mp_size_t argument controls sign of converted double
 */
HsDouble
integer_gmp_mpn_get_d (const mp_limb_t sp[], const mp_size_t sn,
                       const HsInt exponent)
{
  if (sn == 0)
    return 0.0; // should not happen

  if (sn == 1 && sp[0] == 0)
    return 0.0;

  __mpz_struct const mpz = {
    ._mp_alloc = abs(sn),
    ._mp_size  = sn,
    ._mp_d = (mp_limb_t*)sp
  };

  if (!exponent)
    return mpz_get_d(&mpz);

  long e = 0;
  double d = mpz_get_d_2exp (&e, &mpz);

  // TODO: over/underflow handling?
  return ldexp(d, e+exponent);
}

mp_limb_t
integer_gmp_gcd_word(const mp_limb_t x, const mp_limb_t y)
{
  if (!x) return y;
  if (!y) return x;

  return mpn_gcd_1(&x, 1, y);
}

mp_limb_t
integer_gmp_mpn_gcd_1(const mp_limb_t x[], const mp_size_t xn,
                      const mp_limb_t y)
{
  assert (xn > 0);
  assert (xn == 1 || y != 0);

  if (xn == 1)
    return integer_gmp_gcd_word(x[0], y);

  return mpn_gcd_1(x, xn, y);
}


mp_size_t
integer_gmp_mpn_gcd(mp_limb_t r[],
                    const mp_limb_t x0[], const mp_size_t xn,
                    const mp_limb_t y0[], const mp_size_t yn)
{
  assert (xn >= yn);
  assert (yn > 0);
  assert (xn == yn || yn > 1 || y0[0] != 0);
  /* post-condition: rn <= xn */

  if (yn == 1) {
    if (y0[0]) {
      r[0] = integer_gmp_mpn_gcd_1(x0, xn, y0[0]);
      return 1;
    } else { /* {y0,yn} == 0 */
      assert (xn==yn); /* NB: redundant assertion */
      memcpy(r, x0, xn*sizeof(mp_limb_t));
      return xn;
    }
  } else {
    // mpn_gcd() seems to require non-trivial normalization of its
    // input arguments (which does not seem to be documented anywhere,
    // see source of mpz_gcd() for more details), so we resort to just
    // use mpz_gcd() which does the tiresome normalization for us at
    // the cost of a few additional temporary buffer allocations in
    // C-land.

    const mpz_t op1 = {{
      ._mp_alloc = xn,
      ._mp_size  = xn,
      ._mp_d = (mp_limb_t*)x0
      }};

    const mpz_t op2 = {{
      ._mp_alloc = yn,
      ._mp_size  = yn,
      ._mp_d = (mp_limb_t*)y0
      }};

    mpz_t rop;
    mpz_init (rop);

    mpz_gcd(rop, op1, op2);

    const mp_size_t rn = rop[0]._mp_size;
    assert(rn > 0);
    assert(rn <= xn);

    /* the allocation/memcpy of the result can be neglectable since
       mpz_gcd() already has to allocate other temporary buffers
       anyway */
    memcpy(r, rop[0]._mp_d, rn*sizeof(mp_limb_t));

    mpz_clear(rop);

    return rn;
  }
}

/* Truncating (i.e. rounded towards zero) integer division-quotient of MPN */
void
integer_gmp_mpn_tdiv_q (mp_limb_t q[],
                        const mp_limb_t n[], const mp_size_t nn,
                        const mp_limb_t d[], const mp_size_t dn)
{
  /* qn = 1+nn-dn; rn = dn */
  assert(nn>=dn);

  if (dn > 128) {
    // Use temporary heap allocated throw-away buffer for MPNs larger
    // than 1KiB for 64bit-sized limbs (larger than 512bytes for
    // 32bit-sized limbs)
    mp_limb_t *const r = malloc(dn*sizeof(mp_limb_t));
    mpn_tdiv_qr(q, r, 0, n, nn, d, dn);
    free (r);
  } else { // allocate smaller arrays on the stack
    mp_limb_t r[dn];
    mpn_tdiv_qr(q, r, 0, n, nn, d, dn);
  }
}

/* Truncating (i.e. rounded towards zero) integer division-remainder of MPNs */
void
integer_gmp_mpn_tdiv_r (mp_limb_t r[],
                        const mp_limb_t n[], const mp_size_t nn,
                        const mp_limb_t d[], const mp_size_t dn)
{
  /* qn = 1+nn-dn; rn = dn */
  assert(nn>=dn);
  const mp_size_t qn = 1+nn-dn;

  if (qn > 128) {
    // Use temporary heap allocated throw-away buffer for MPNs larger
    // than 1KiB for 64bit-sized limbs (larger than 512bytes for
    // 32bit-sized limbs)
    mp_limb_t *const q = malloc(qn*sizeof(mp_limb_t));
    mpn_tdiv_qr(q, r, 0, n, nn, d, dn);
    free (q);
  } else { // allocate smaller arrays on the stack
    mp_limb_t q[qn];
    mpn_tdiv_qr(q, r, 0, n, nn, d, dn);
  }
}
