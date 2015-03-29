/*
 * `integer-gmp` GMP FFI wrappers
 *
 * Copyright (c) 2014, Herbert Valerio Riedel <hvr@gnu.org>
 *
 * BSD3 licensed, see ../LICENSE file for details
 *
 */

#define _ISOC99_SOURCE

#include "HsFFI.h"
#include "MachDeps.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <math.h>
#include <float.h>
#include <stdio.h>

#include <gmp.h>


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

// Turn a (const) {xp,xn} pair into static initializer
#define CONST_MPZ_INIT(xp,xn) \
  {{ ._mp_alloc = 0, ._mp_size  = (xn), ._mp_d = (mp_limb_t*)(xp) }}

// Test if {sp,sn} represents a zero value
static inline int
mp_limb_zero_p(const mp_limb_t sp[], mp_size_t sn)
{
  return !sn || ((sn == 1 || sn == -1) && !sp[0]);
}

static inline mp_size_t
mp_size_abs(const mp_size_t x)
{
  return x>=0 ? x : -x;
}

static inline mp_size_t
mp_size_min(const mp_size_t x, const mp_size_t y)
{
  return x<y ? x : y;
}

static inline mp_size_t
mp_size_minabs(const mp_size_t x, const mp_size_t y)
{
  return mp_size_min(mp_size_abs(x), mp_size_abs(y));
}

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

/* Convert bignum to a `double`, truncating if necessary
 * (i.e. rounding towards zero).
 *
 * sign of mp_size_t argument controls sign of converted double
 */
HsDouble
integer_gmp_mpn_get_d (const mp_limb_t sp[], const mp_size_t sn,
                       const HsInt exponent)
{
  if (mp_limb_zero_p(sp, sn))
    return 0.0;

  const mpz_t mpz = CONST_MPZ_INIT(sp, sn);

  if (!exponent)
    return mpz_get_d(mpz);

  long e = 0;
  double d = mpz_get_d_2exp (&e, mpz);

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

    const mpz_t op1 = CONST_MPZ_INIT(x0, xn);
    const mpz_t op2 = CONST_MPZ_INIT(y0, yn);

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

/* wraps mpz_gcdext()
 *
 * Set g to the greatest common divisor of x and y, and in addition
 * set s and t to coefficients satisfying x*s + y*t = g.
 *
 * The {gp,gn} array is zero-padded (as otherwise 'gn' can't be
 * reconstructed).
 *
 * g must have space for exactly gn=min(xn,yn) limbs.
 * s must have space for at least xn limbs.
 *
 * return value: signed 'sn' of {sp,sn}
 */
mp_size_t
integer_gmp_gcdext(mp_limb_t s0[], mp_limb_t g0[],
                   const mp_limb_t x0[], const mp_size_t xn,
                   const mp_limb_t y0[], const mp_size_t yn)
{
  const mp_size_t gn0 = mp_size_minabs(xn, yn);
  const mpz_t x = CONST_MPZ_INIT(x0, mp_limb_zero_p(x0,xn) ? 0 : xn);
  const mpz_t y = CONST_MPZ_INIT(y0, mp_limb_zero_p(y0,yn) ? 0 : yn);

  mpz_t g, s;
  mpz_init (g);
  mpz_init (s);

  mpz_gcdext (g, s, NULL, x, y);

  const mp_size_t gn = g[0]._mp_size;
  assert(0 <= gn && gn <= gn0);
  memset(g0, 0, gn0*sizeof(mp_limb_t));
  memcpy(g0, g[0]._mp_d, gn*sizeof(mp_limb_t));
  mpz_clear (g);

  const mp_size_t ssn = s[0]._mp_size;
  const mp_size_t sn  = mp_size_abs(ssn);
  assert(sn <= xn);
  memcpy(s0, s[0]._mp_d, sn*sizeof(mp_limb_t));
  mpz_clear (s);

  if (!sn) {
    s0[0] = 0;
    return 1;
  }

  return ssn;
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


/* Wraps GMP's 'mpz_sizeinbase()' function */
HsWord
integer_gmp_mpn_sizeinbase(const mp_limb_t s[], const mp_size_t sn,
                           const HsInt base)
{
  assert (2 <= base && base <= 256);

  if (mp_limb_zero_p(s,sn)) return 1;

  const mpz_t zs = CONST_MPZ_INIT(s, sn);

  return mpz_sizeinbase(zs, base);
}

/* Single-limb version of 'integer_gmp_mpn_sizeinbase()' */
HsWord
integer_gmp_mpn_sizeinbase1(const mp_limb_t s, const HsInt base)
{
  return s ? integer_gmp_mpn_sizeinbase(&s, 1, base) : 1;
}

/* Wrapper around GMP's 'mpz_export()' function */
HsWord
integer_gmp_mpn_export(const mp_limb_t s[], const mp_size_t sn,
                       void *destptr, HsInt destofs, HsInt msbf)
{
  /* TODO: implement w/o GMP, c.f. 'integer_gmp_mpn_import()' */
  assert (msbf == 0 || msbf == 1);

  if (mp_limb_zero_p(s,sn)) return 0;

  const mpz_t zs = CONST_MPZ_INIT(s, sn);

  size_t written = 0;

  // mpz_export (void *rop, size_t *countp, int order, size_t size, int endian,
  //             size_t nails, const mpz_t op)
  (void) mpz_export(((char *)destptr)+destofs, &written, !msbf ? -1 : 1,
                    /* size */ 1, /* endian */ 0, /* nails */ 0, zs);

  return written;
}

/* Single-limb version of 'integer_gmp_mpn_export()' */
HsWord
integer_gmp_mpn_export1(const mp_limb_t s,
                        void *destptr, const HsInt destofs, const HsInt msbf)
{
  /* TODO: implement w/o GMP */
  return integer_gmp_mpn_export(&s, 1, destptr, destofs, msbf);
}

/* Import single limb from memory location
 *
 * We can't use GMP's 'mpz_import()'
 */
HsWord
integer_gmp_mpn_import1(const uint8_t *srcptr, const HsWord srcofs,
                        const HsWord srclen, const HsInt msbf)
{
  assert (msbf == 0 || msbf == 1);
  assert (srclen <= SIZEOF_HSWORD);

  srcptr += srcofs;

  HsWord result = 0;

  if (msbf)
    for (unsigned i = 0; i < srclen; ++i)
      result |= (HsWord)srcptr[i] << ((srclen-i-1)*8);
  else // lsbf
    for (unsigned i = 0; i < srclen; ++i)
      result |= (HsWord)srcptr[i] << (i*8);

  return result;
}

/* import into mp_limb_t[] from memory location */
void
integer_gmp_mpn_import(mp_limb_t * restrict r, const uint8_t * restrict srcptr,
                       const HsWord srcofs, const HsWord srclen,
                       const HsInt msbf)
{
  assert (msbf == 0 || msbf == 1);

  srcptr += srcofs;

  const unsigned  limb_cnt_rem = srclen % SIZEOF_HSWORD;
  const mp_size_t limb_cnt     = srclen / SIZEOF_HSWORD;

  if (msbf) {
    if (limb_cnt_rem) { // partial limb
      r[limb_cnt] = integer_gmp_mpn_import1(srcptr, 0, limb_cnt_rem, 1);
      srcptr += limb_cnt_rem;
    }

    for (unsigned ri = 0; ri < limb_cnt; ++ri) {
      r[limb_cnt-ri-1] = integer_gmp_mpn_import1(srcptr, 0, SIZEOF_HSWORD, 1);
      srcptr += SIZEOF_HSWORD;
    }
  } else { // lsbf
    for (unsigned ri = 0; ri < limb_cnt; ++ri) {
      r[ri] = integer_gmp_mpn_import1(srcptr, 0, SIZEOF_HSWORD, 0);
      srcptr += SIZEOF_HSWORD;
    }

    if (limb_cnt_rem) // partial limb
      r[limb_cnt] = integer_gmp_mpn_import1(srcptr, 0, limb_cnt_rem, 0);
  }
}

/* Scan for first non-zero byte starting at srcptr[srcofs], ending at
 * srcptr[srcofs+srclen-1];
 *
 * If no non-zero byte found, returns srcofs+srclen; otherwise returns
 * index of srcptr where first non-zero byte was found.
 */
HsWord
integer_gmp_scan_nzbyte(const uint8_t *srcptr,
                        const HsWord srcofs, const HsWord srclen)
{
  // TODO: consider implementing this function in Haskell-land
  srcptr += srcofs;

  for (unsigned i = 0; i < srclen; ++i)
    if (srcptr[i])
      return srcofs+i;

  return srcofs+srclen;
}

/* Reverse scan for non-zero byte
 * starting at srcptr[srcofs+srclen-1], ending at srcptr[srcofs].
 *
 * Returns new length srclen1 such that srcptr[srcofs+i] == 0 for
 * srclen1 <= i < srclen.
 */
HsWord
integer_gmp_rscan_nzbyte(const uint8_t *srcptr,
                         const HsWord srcofs, const HsWord srclen)
{
  // TODO: consider implementing this function in Haskell-land
  srcptr += srcofs;

  for (unsigned i = srclen; i > 0; --i)
    if (srcptr[i-1])
      return i;

  return 0;
}

/* wrapper around mpz_probab_prime_p */
HsInt
integer_gmp_test_prime(const mp_limb_t s[], const mp_size_t sn, const HsInt rep)
{
  if (mp_limb_zero_p(s,sn)) return 0;

  const mpz_t sz = CONST_MPZ_INIT(s, sn);

  // int mpz_probab_prime_p (const mpz_t n, int reps)
  return mpz_probab_prime_p(sz, rep);
}

/* wrapper around mpz_probab_prime_p */
HsInt
integer_gmp_test_prime1(const mp_limb_t limb, const HsInt rep)
{
  if (!limb) return 0;

  return integer_gmp_test_prime(&limb, 1, rep);
}

/* wrapper around mpz_nextprime()
 *
 * Stores next prime (relative to {sp,sn}) in {rp,sn}.
 * Return value is most significant limb of {rp,sn+1}.
 */
mp_limb_t
integer_gmp_next_prime(mp_limb_t rp[], const mp_limb_t sp[],
                       const mp_size_t sn)
{
  assert (sn>=0);

  if (!sn) return 2;
  if (sn == 1 && sp[0] < 2) {
    rp[0] = 2;
    return 0;
  }

  const mpz_t op = CONST_MPZ_INIT(sp, sn);

  mpz_t rop;
  mpz_init (rop);
  mpz_nextprime (rop, op);

  const mp_size_t rn = rop[0]._mp_size;

  // copy result into {rp,sn} buffer
  assert (rn == sn || rn == sn+1);
  memcpy(rp, rop[0]._mp_d, sn*sizeof(mp_limb_t));
  const mp_limb_t result = rn>sn ? rop[0]._mp_d[sn] : 0;

  mpz_clear (rop);

  return result;
}

/* wrapper around mpz_nextprime()
 *
 * returns next prime modulo 2^GMP_LIMB_BITS
 */
mp_limb_t
integer_gmp_next_prime1(const mp_limb_t limb)
{
  if (limb < 2) return 2;

  const mpz_t op = CONST_MPZ_INIT(&limb, 1);

  mpz_t rop;
  mpz_init (rop);
  mpz_nextprime (rop, op);
  assert (rop[0]._mp_size > 0);
  const mp_limb_t result = rop[0]._mp_d[0];
  mpz_clear (rop);

  return result;
}

/* wrapper around mpz_powm()
 *
 * Store '(B^E) mod M' in {rp,rn}
 *
 * rp must have allocated mn limbs; This function's return value is
 * the actual number rn (0 < rn <= mn) of limbs written to the rp limb-array.
 *
 * bn and en are allowed to be negative to denote negative numbers
 */
mp_size_t
integer_gmp_powm(mp_limb_t rp[], // result
                 const mp_limb_t bp[], const mp_size_t bn, // base
                 const mp_limb_t ep[], const mp_size_t en, // exponent
                 const mp_limb_t mp[], const mp_size_t mn) // mod
{
  assert(!mp_limb_zero_p(mp,mn));

  if ((mn == 1 || mn == -1) && mp[0] == 1) {
    rp[0] = 0;
    return 1;
  }

  if (mp_limb_zero_p(ep,en)) {
    rp[0] = 1;
    return 1;
  }

  const mpz_t b = CONST_MPZ_INIT(bp, mp_limb_zero_p(bp,bn) ? 0 : bn);
  const mpz_t e = CONST_MPZ_INIT(ep, mp_limb_zero_p(ep,en) ? 0 : en);
  const mpz_t m = CONST_MPZ_INIT(mp, mn);

  mpz_t r;
  mpz_init (r);

  mpz_powm(r, b, e, m);

  const mp_size_t rn = r[0]._mp_size;

  if (rn) {
    assert(0 < rn && rn <= mn);
    memcpy(rp, r[0]._mp_d, rn*sizeof(mp_limb_t));
  }

  mpz_clear (r);

  if (!rn) {
    rp[0] = 0;
    return 1;
  }

  return rn;
}

/* version of integer_gmp_powm() for single-limb moduli */
mp_limb_t
integer_gmp_powm1(const mp_limb_t bp[], const mp_size_t bn, // base
                  const mp_limb_t ep[], const mp_size_t en, // exponent
                  const mp_limb_t m0) // mod
{
  assert(m0);

  if (m0==1) return 0;
  if (mp_limb_zero_p(ep,en)) return 1;

  const mpz_t b = CONST_MPZ_INIT(bp, mp_limb_zero_p(bp,bn) ? 0 : bn);
  const mpz_t e = CONST_MPZ_INIT(ep, en);
  const mpz_t m = CONST_MPZ_INIT(&m0, !!m0);

  mpz_t r;
  mpz_init (r);
  mpz_powm(r, b, e, m);

  assert(r[0]._mp_size == 0 || r[0]._mp_size == 1);
  const mp_limb_t result = r[0]._mp_size ? r[0]._mp_d[0] : 0;

  mpz_clear (r);

  return result;
}

/* version of integer_gmp_powm() for single-limb arguments */
mp_limb_t
integer_gmp_powm_word(const mp_limb_t b0, // base
                      const mp_limb_t e0, // exponent
                      const mp_limb_t m0) // mod
{
  return integer_gmp_powm1(&b0, !!b0, &e0, !!e0, m0);
}


/* wrapper around mpz_invert()
 *
 * Store '(1/X) mod abs(M)' in {rp,rn}
 *
 * rp must have allocated mn limbs; This function's return value is
 * the actual number rn (0 < rn <= mn) of limbs written to the rp limb-array.
 *
 * Returns 0 if inverse does not exist.
 */
mp_size_t
integer_gmp_invert(mp_limb_t rp[], // result
                   const mp_limb_t xp[], const mp_size_t xn, // base
                   const mp_limb_t mp[], const mp_size_t mn) // mod
{
  if (mp_limb_zero_p(xp,xn)
      || mp_limb_zero_p(mp,mn)
      || ((mn == 1 || mn == -1) && mp[0] == 1)) {
    rp[0] = 0;
    return 1;
  }

  const mpz_t x = CONST_MPZ_INIT(xp, xn);
  const mpz_t m = CONST_MPZ_INIT(mp, mn);

  mpz_t r;
  mpz_init (r);

  const int inv_exists = mpz_invert(r, x, m);

  const mp_size_t rn = inv_exists ? r[0]._mp_size : 0;

  if (rn) {
    assert(0 < rn && rn <= mn);
    memcpy(rp, r[0]._mp_d, rn*sizeof(mp_limb_t));
  }

  mpz_clear (r);

  if (!rn) {
    rp[0] = 0;
    return 1;
  }

  return rn;
}


/* Version of integer_gmp_invert() operating on single limbs */
mp_limb_t
integer_gmp_invert_word(const mp_limb_t x0, const mp_limb_t m0)
{
  if (!x0 || m0<=1) return 0;
  if (x0 == 1) return 1;

  const mpz_t x = CONST_MPZ_INIT(&x0, 1);
  const mpz_t m = CONST_MPZ_INIT(&m0, 1);

  mpz_t r;
  mpz_init (r);

  const int inv_exists = mpz_invert(r, x, m);
  const mp_size_t rn = inv_exists ? r[0]._mp_size : 0;

  assert (rn == 0 || rn == 1);
  const mp_limb_t r0 = rn ? r[0]._mp_d[0] : 0;

  mpz_clear (r);

  return r0;
}


/* Wrappers for GMP 4.x compat
 *
 * In GMP 5.0 the following operations were added:
 *
 *  mpn_sqr, mpn_and_n, mpn_ior_n, mpn_xor_n, mpn_nand_n, mpn_nior_n,
 *  mpn_xnor_n, mpn_andn_n, mpn_iorn_n, mpn_com, mpn_neg, mpn_copyi,
 *  mpn_copyd, mpn_zero
 *
 * We use some of those, but for GMP 4.x compatibility we need to
 * emulate those (while incurring some overhead).
 */
#if __GNU_MP_VERSION < 5

#define MPN_LOGIC_OP_WRAPPER(MPN_WRAPPER, MPZ_OP) \
void                                                               \
MPN_WRAPPER(mp_limb_t *rp, const mp_limb_t *s1p,                   \
            const mp_limb_t *s2p, mp_size_t n)                     \
{                                                                  \
  assert(n > 0);                                                   \
                                                                   \
  const mpz_t s1 = CONST_MPZ_INIT(s1p, n);                         \
  const mpz_t s2 = CONST_MPZ_INIT(s2p, n);                         \
                                                                   \
  mpz_t r;                                                         \
  mpz_init (r);                                                    \
  MPZ_OP (r, s1, s2);                                              \
                                                                   \
  const mp_size_t rn = r[0]._mp_size;                              \
  memset (rp, 0, n*sizeof(mp_limb_t));                             \
  memcpy (rp, r[0]._mp_d, mp_size_minabs(rn,n)*sizeof(mp_limb_t)); \
                                                                   \
  mpz_clear (r);                                                   \
}

static void
__mpz_andn(mpz_t r, const mpz_t s1, const mpz_t s2)
{
  mpz_t s2c;
  mpz_init (s2c);
  mpz_com (s2c, s2);
  mpz_and (r, s1, s2c);
  mpz_clear (s2c);
}

MPN_LOGIC_OP_WRAPPER(integer_gmp_mpn_and_n,  mpz_and)
MPN_LOGIC_OP_WRAPPER(integer_gmp_mpn_andn_n, __mpz_andn)
MPN_LOGIC_OP_WRAPPER(integer_gmp_mpn_ior_n,  mpz_ior)
MPN_LOGIC_OP_WRAPPER(integer_gmp_mpn_xor_n,  mpz_xor)

#else /* __GNU_MP_VERSION >= 5 */
void
integer_gmp_mpn_and_n(mp_limb_t *rp, const mp_limb_t *s1p,
                      const mp_limb_t *s2p, mp_size_t n)
{
  mpn_and_n(rp, s1p, s2p, n);
}

void
integer_gmp_mpn_andn_n(mp_limb_t *rp, const mp_limb_t *s1p,
                      const mp_limb_t *s2p, mp_size_t n)
{
  mpn_andn_n(rp, s1p, s2p, n);
}

void
integer_gmp_mpn_ior_n(mp_limb_t *rp, const mp_limb_t *s1p,
                      const mp_limb_t *s2p, mp_size_t n)
{
  mpn_ior_n(rp, s1p, s2p, n);
}

void
integer_gmp_mpn_xor_n(mp_limb_t *rp, const mp_limb_t *s1p,
                      const mp_limb_t *s2p, mp_size_t n)
{
  mpn_xor_n(rp, s1p, s2p, n);
}
#endif
