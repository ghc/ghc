/* _gmp_rand (rp, state, nbits) -- Generate a random bitstream of
   length NBITS in RP.  RP must have enough space allocated to hold
   NBITS.

Copyright (C) 1999, 2000 Free Software Foundation, Inc.

This file is part of the GNU MP Library.

The GNU MP Library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 2.1 of the License, or (at your
option) any later version.

The GNU MP Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the GNU MP Library; see the file COPYING.LIB.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
MA 02111-1307, USA. */

#include "gmp.h"
#include "gmp-impl.h"
#include "longlong.h"

/* For linear congruential (LC), we use one of algorithms (1) or (2).
   (gmp-3.0 uses algorithm (1) with 'm' as a power of 2.)

LC algorithm (1).

	X = (aX + c) mod m

[D. Knuth, "The Art of Computer Programming: Volume 2, Seminumerical Algorithms",
Third Edition, Addison Wesley, 1998, pp. 184-185.]

	X is the seed and the result
	a is chosen so that
	    a mod 8 = 5 [3.2.1.2] and [3.2.1.3]
	    .01m < a < .99m
	    its binary or decimal digits is not a simple, regular pattern
	    it has no large quotients when Euclid's algorithm is used to find
	      gcd(a, m) [3.3.3]
	    it passes the spectral test [3.3.4]
	    it passes several tests of [3.3.2]
	c has no factor in common with m (c=1 or c=a can be good)
	m is large (2^30)
	  is a power of 2 [3.2.1.1]

The least significant digits of the generated number are not very
random.  It should be regarded as a random fraction X/m.  To get a
random integer between 0 and n-1, multiply X/m by n and truncate.
(Don't use X/n [ex 3.4.1-3])

The ``accuracy'' in t dimensions is one part in ``the t'th root of m'' [3.3.4].

Don't generate more than about m/1000 numbers without changing a, c, or m.

The sequence length depends on chosen a,c,m.


LC algorithm (2).

	X = a * (X mod q) - r * (long) (X/q)
	if X<0 then X+=m

[Knuth, pp. 185-186.]

	X is the seed and the result
	  as a seed is nonzero and less than m
	a is a primitive root of m (which means that a^2 <= m)
	q is (long) m / a
	r is m mod a
	m is a prime number near the largest easily computed integer

which gives

	X = a * (X % ((long) m / a)) -
	    (M % a) * ((long) (X / ((long) m / a)))

Since m is prime, the least-significant bits of X are just as random as
the most-significant bits. */

/* Blum, Blum, and Shub.

   [Bruce Schneier, "Applied Cryptography", Second Edition, John Wiley
   & Sons, Inc., 1996, pp. 417-418.]

   "Find two large prime numbers, p and q, which are congruent to 3
   modulo 4.  The product of those numbers, n, is a blum integer.
   Choose another random integer, x, which is relatively prime to n.
   Compute
	x[0] = x^2 mod n
   That's the seed for the generator."

   To generate a random bit, compute
	x[i] = x[i-1]^2 mod n
   The least significant bit of x[i] is the one we want.

   We can use more than one bit from x[i], namely the
	log2(bitlength of x[i])
   least significant bits of x[i].

   So, for a 32-bit seed we get 5 bits per computation.

   The non-predictability of this generator is based on the difficulty
   of factoring n.
 */

/* -------------------------------------------------- */

/* lc (rp, state) -- Generate next number in LC sequence.  Return the
   number of valid bits in the result.  NOTE: If 'm' is a power of 2
   (m2exp != 0), discard the lower half of the result.  */

static
unsigned long int
#if __STDC__
lc (mp_ptr rp, gmp_randstate_t rstate)
#else
lc (rp, rstate)
     mp_ptr rp;
     gmp_randstate_t rstate;
#endif
{
  mp_ptr tp, seedp, ap;
  mp_size_t ta;
  mp_size_t tn, seedn, an;
  mp_size_t retval;
  int shiftcount = 0;
  unsigned long int m2exp;
  mp_limb_t c;
  TMP_DECL (mark);

  m2exp = rstate->algdata.lc->m2exp;
  c = (mp_limb_t) rstate->algdata.lc->c;

  seedp = PTR (rstate->seed);
  seedn = SIZ (rstate->seed);

  if (seedn == 0)
    {
      /* Seed is 0.  Result is C % M.  */
      *rp = c;

      if (m2exp != 0)
	{
	  /* M is a power of 2.  */
	  if (m2exp < BITS_PER_MP_LIMB)
	    {
	      /* Only necessary when M may be smaller than C.  */
	      *rp &= (((mp_limb_t) 1 << m2exp) - 1);
	    }
	}
      else
	{
	  /* M is not a power of 2.  */
	  abort ();		/* FIXME.  */
	}

      /* Save result as next seed.  */
      *seedp = *rp;
      SIZ (rstate->seed) = 1;
      return BITS_PER_MP_LIMB;
    }

  ap = PTR (rstate->algdata.lc->a);
  an = SIZ (rstate->algdata.lc->a);

  /* Allocate temporary storage.  Let there be room for calculation of
     (A * seed + C) % M, or M if bigger than that.  */

  ASSERT_ALWAYS (m2exp != 0);	/* FIXME.  */

  TMP_MARK (mark);
  ta = an + seedn + 1;
  tp = (mp_ptr) TMP_ALLOC (ta * BYTES_PER_MP_LIMB);
  MPN_ZERO (tp, ta);

  /* t = a * seed */
  if (seedn >= an)
    mpn_mul_basecase (tp, seedp, seedn, ap, an);
  else
    mpn_mul_basecase (tp, ap, an, seedp, seedn);
  tn = an + seedn;

  /* t = t + c */
  mpn_incr_u (tp, c);

  /* t = t % m */
  if (m2exp != 0)
    {
      /* M is a power of 2.  The mod operation is trivial.  */

      tp[m2exp / BITS_PER_MP_LIMB] &= ((mp_limb_t) 1 << m2exp % BITS_PER_MP_LIMB) - 1;
      tn = (m2exp + BITS_PER_MP_LIMB - 1) / BITS_PER_MP_LIMB;
    }
  else
    {
      abort ();			/* FIXME.  */
    }

  /* Save result as next seed.  */
  MPN_COPY (PTR (rstate->seed), tp, tn);
  SIZ (rstate->seed) = tn;

  if (m2exp != 0)
    {
      /* Discard the lower half of the result.  */
      unsigned long int discardb = m2exp / 2;
      mp_size_t discardl = discardb / BITS_PER_MP_LIMB;

      tn -= discardl;
      if (tn > 0)
	{
	  if (discardb % BITS_PER_MP_LIMB != 0)
	    {
	      mpn_rshift (tp, tp + discardl, tn, discardb % BITS_PER_MP_LIMB);
	      MPN_COPY (rp, tp, (discardb + BITS_PER_MP_LIMB -1) / BITS_PER_MP_LIMB);
	    }
	  else			/* Even limb boundary.  */
	    MPN_COPY_INCR (rp, tp + discardl, tn);
	}
    }
  else
    {
      MPN_COPY (rp, tp, tn);
    }

  TMP_FREE (mark);

  /* Return number of valid bits in the result.  */
  if (m2exp != 0)
    retval = (m2exp + 1) / 2;
  else
    retval = SIZ (rstate->algdata.lc->m) * BITS_PER_MP_LIMB - shiftcount;
  return retval;
}

#ifdef RAWRANDEBUG
/* Set even bits to EVENBITS and odd bits to ! EVENBITS in RP.
   Number of bits is m2exp in state.  */
/* FIXME: Remove.  */
unsigned long int
lc_test (mp_ptr rp, gmp_randstate_t s, const int evenbits)
{
  unsigned long int rn, nbits;
  int f;

  nbits = s->algdata.lc->m2exp / 2;
  rn = nbits / BITS_PER_MP_LIMB + (nbits % BITS_PER_MP_LIMB != 0);
  MPN_ZERO (rp, rn);

  for (f = 0; f < nbits; f++)
    {
      mpn_lshift (rp, rp, rn, 1);
      if (f % 2 == ! evenbits)
	rp[0] += 1;
    }

  return nbits;
}
#endif /* RAWRANDEBUG */

void
#if __STDC__
_gmp_rand (mp_ptr rp, gmp_randstate_t rstate, unsigned long int nbits)
#else
_gmp_rand (rp, rstate, nbits)
     mp_ptr rp;
     gmp_randstate_t rstate;
     unsigned long int nbits;
#endif
{
  mp_size_t rn;			/* Size of R.  */

  rn = (nbits + BITS_PER_MP_LIMB - 1) / BITS_PER_MP_LIMB;

  switch (rstate->alg)
    {
    case GMP_RAND_ALG_LC:
      {
	unsigned long int rbitpos;
	int chunk_nbits;
	mp_ptr tp;
	mp_size_t tn;
	TMP_DECL (lcmark);

	TMP_MARK (lcmark);

	chunk_nbits = rstate->algdata.lc->m2exp / 2;
	tn = (chunk_nbits + BITS_PER_MP_LIMB - 1) / BITS_PER_MP_LIMB;

	tp = (mp_ptr) TMP_ALLOC (tn * BYTES_PER_MP_LIMB);

	rbitpos = 0;
	while (rbitpos + chunk_nbits <= nbits)
	  {
	    mp_ptr r2p = rp + rbitpos / BITS_PER_MP_LIMB;

	    if (rbitpos % BITS_PER_MP_LIMB != 0)
	      {
		mp_limb_t savelimb, rcy;
		/* Target of of new chunk is not bit aligned.  Use temp space
		   and align things by shifting it up.  */
		lc (tp, rstate);
		savelimb = r2p[0];
		rcy = mpn_lshift (r2p, tp, tn, rbitpos % BITS_PER_MP_LIMB);
		r2p[0] |= savelimb;
/* bogus */	if ((chunk_nbits % BITS_PER_MP_LIMB + rbitpos % BITS_PER_MP_LIMB)
		    > BITS_PER_MP_LIMB)
		  r2p[tn] = rcy;
	      }
	    else
	      {
		/* Target of of new chunk is bit aligned.  Let `lc' put bits
		   directly into our target variable.  */
		lc (r2p, rstate);
	      }
	    rbitpos += chunk_nbits;
	  }

	/* Handle last [0..chunk_nbits) bits.  */
	if (rbitpos != nbits)
	  {
	    mp_ptr r2p = rp + rbitpos / BITS_PER_MP_LIMB;
	    int last_nbits = nbits - rbitpos;
	    tn = (last_nbits + BITS_PER_MP_LIMB - 1) / BITS_PER_MP_LIMB;
	    lc (tp, rstate);
	    if (rbitpos % BITS_PER_MP_LIMB != 0)
	      {
		mp_limb_t savelimb, rcy;
		/* Target of of new chunk is not bit aligned.  Use temp space
		   and align things by shifting it up.  */
		savelimb = r2p[0];
		rcy = mpn_lshift (r2p, tp, tn, rbitpos % BITS_PER_MP_LIMB);
		r2p[0] |= savelimb;
		if (rbitpos + tn * BITS_PER_MP_LIMB - rbitpos % BITS_PER_MP_LIMB < nbits)
		  r2p[tn] = rcy;
	      }
	    else
	      {
		MPN_COPY (r2p, tp, tn);
	      }
	    /* Mask off top bits if needed.  */
	    if (nbits % BITS_PER_MP_LIMB != 0)
	      rp[nbits / BITS_PER_MP_LIMB]
		&= ~ ((~(mp_limb_t) 0) << nbits % BITS_PER_MP_LIMB);
	  }

	TMP_FREE (lcmark);
	break;
      }

    default:
      gmp_errno |= GMP_ERROR_UNSUPPORTED_ARGUMENT;
      break;
    }
}
