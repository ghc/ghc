/* mpz_powm(res,base,exp,mod) -- Set RES to (base**exp) mod MOD.

Copyright (C) 1991, 1993, 1994, 1996, 1997, 2000 Free Software Foundation, Inc.
Contributed by Paul Zimmermann.

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
#ifdef BERKELEY_MP
#include "mp.h"
#endif


/* set c <- (a*b)/R^n mod m c has to have at least (2n) allocated limbs */
static void
#if __STDC__
mpz_redc (mpz_ptr c, mpz_srcptr a, mpz_srcptr b, mpz_srcptr m, mp_limb_t Nprim)
#else
mpz_redc (c, a, b, m, Nprim)
     mpz_ptr c;
     mpz_srcptr a;
     mpz_srcptr b;
     mpz_srcptr m;
     mp_limb_t Nprim;
#endif
{
  mp_ptr cp, mp = PTR (m);
  mp_limb_t cy, cout = 0;
  mp_limb_t q;
  size_t j, n = ABSIZ (m);

  ASSERT (ALLOC (c) >= 2 * n);

  mpz_mul (c, a, b);
  cp = PTR (c);
  j = ABSIZ (c);
  MPN_ZERO (cp + j, 2 * n - j);
  for (j = 0; j < n; j++)
    {
      q = cp[0] * Nprim;
      cy = mpn_addmul_1 (cp, mp, n, q);
      cout += mpn_add_1 (cp + n, cp + n, n - j, cy);
      cp++;
    }
  cp -= n;
  if (cout)
    {
      cy = cout - mpn_sub_n (cp, cp + n, mp, n);
      while (cy)
	cy -= mpn_sub_n (cp, cp, mp, n);
    }
  else
    MPN_COPY (cp, cp + n, n);
  MPN_NORMALIZE (cp, n);
  SIZ (c) = SIZ (c) < 0 ? -n : n;
}

/* average number of calls to redc for an exponent of n bits
   with the sliding window algorithm of base 2^k: the optimal is
   obtained for the value of k which minimizes 2^(k-1)+n/(k+1):

   n\k    4     5     6     7     8
   128    156*  159   171   200   261
   256    309   307*  316   343   403
   512    617   607*  610   632   688
   1024   1231  1204  1195* 1207  1256
   2048   2461  2399  2366  2360* 2396
   4096   4918  4787  4707  4665* 4670
*/

#ifndef BERKELEY_MP
void
#if __STDC__
mpz_powm (mpz_ptr res, mpz_srcptr base, mpz_srcptr e, mpz_srcptr mod)
#else
mpz_powm (res, base, e, mod)
     mpz_ptr res;
     mpz_srcptr base;
     mpz_srcptr e;
     mpz_srcptr mod;
#endif
#else /* BERKELEY_MP */
void
#if __STDC__
pow (mpz_srcptr base, mpz_srcptr e, mpz_srcptr mod, mpz_ptr res)
#else
pow (base, e, mod, res)
     mpz_srcptr base;
     mpz_srcptr e;
     mpz_srcptr mod;
     mpz_ptr res;
#endif
#endif /* BERKELEY_MP */
{
  mp_limb_t invm, *ep, c, mask;
  mpz_t xx, *g;
  mp_size_t n, i, K, j, l, k;
  int sh;
  int use_redc;

#ifdef POWM_DEBUG
  mpz_t exp;
  mpz_init (exp);
#endif

  n = ABSIZ (mod);

  if (n == 0)
    DIVIDE_BY_ZERO;

  if (SIZ (e) == 0)
    {
      /* Exponent is zero, result is 1 mod MOD, i.e., 1 or 0
         depending on if MOD equals 1.  */
      SIZ(res) = (ABSIZ (mod) == 1 && (PTR(mod))[0] == 1) ? 0 : 1;
      PTR(res)[0] = 1;
      return;
    }

  /* Use REDC instead of usual reduction for sizes < POWM_THRESHOLD.
     In REDC each modular multiplication costs about 2*n^2 limbs operations,
     whereas using usual reduction it costs 3*K(n), where K(n) is the cost of a
     multiplication using Karatsuba, and a division is assumed to cost 2*K(n),
     for example using Burnikel-Ziegler's algorithm. This gives a theoretical
     threshold of a*KARATSUBA_SQR_THRESHOLD, with a=(3/2)^(1/(2-ln(3)/ln(2))) ~
     2.66.  */
  /* For now, also disable REDC when MOD is even, as the inverse can't
     handle that.  */

#ifndef POWM_THRESHOLD
#define POWM_THRESHOLD  ((8 * KARATSUBA_SQR_THRESHOLD) / 3)
#endif

  use_redc = (n < POWM_THRESHOLD && PTR(mod)[0] % 2 != 0);
  if (use_redc)
    {
      /* invm = -1/m mod 2^BITS_PER_MP_LIMB, must have m odd */
      modlimb_invert (invm, PTR(mod)[0]);
      invm = -invm;
    }

  /* determines optimal value of k */
  l = ABSIZ (e) * BITS_PER_MP_LIMB; /* number of bits of exponent */
  k = 1;
  K = 2;
  while (2 * l > K * (2 + k * (3 + k)))
    {
      k++;
      K *= 2;
    }

  g = (mpz_t *) (*_mp_allocate_func) (K / 2 * sizeof (mpz_t));
  /* compute x*R^n where R=2^BITS_PER_MP_LIMB */
  mpz_init (g[0]);
  if (use_redc)
    {
      mpz_mul_2exp (g[0], base, n * BITS_PER_MP_LIMB);
      mpz_mod (g[0], g[0], mod);
    }
  else
    mpz_mod (g[0], base, mod);

  /* compute xx^g for odd g < 2^k */
  mpz_init (xx);
  if (use_redc)
    {
      _mpz_realloc (xx, 2 * n);
      mpz_redc (xx, g[0], g[0], mod, invm); /* xx = x^2*R^n */
    }
  else
    {
      mpz_mul (xx, g[0], g[0]);
      mpz_mod (xx, xx, mod);
    }
  for (i = 1; i < K / 2; i++)
    {
      mpz_init (g[i]);
      if (use_redc)
	{
	  _mpz_realloc (g[i], 2 * n);
	  mpz_redc (g[i], g[i - 1], xx, mod, invm); /* g[i] = x^(2i+1)*R^n */
	}
      else
	{
	  mpz_mul (g[i], g[i - 1], xx);
	  mpz_mod (g[i], g[i], mod);
	}
    }

  /* now starts the real stuff */
  mask = (mp_limb_t) ((1<<k) - 1);
  ep = PTR (e);
  i = ABSIZ (e) - 1;			/* current index */
  c = ep[i];				/* current limb */
  count_leading_zeros (sh, c);
  sh = BITS_PER_MP_LIMB - sh;		/* significant bits in ep[i] */
  sh -= k;				/* index of lower bit of ep[i] to take into account */
  if (sh < 0)
    {					/* k-sh extra bits are needed */
      if (i > 0)
	{
	  i--;
	  c = (c << (-sh)) | (ep[i] >> (BITS_PER_MP_LIMB + sh));
	  sh += BITS_PER_MP_LIMB;
	}
    }
  else
    c = c >> sh;
#ifdef POWM_DEBUG
  printf ("-1/m mod 2^%u = %lu\n", BITS_PER_MP_LIMB, invm);
  mpz_set_ui (exp, c);
#endif
  j=0;
  while (c % 2 == 0)
    {
      j++;
      c = (c >> 1);
    }
  mpz_set (xx, g[c >> 1]);
  while (j--)
    {
      if (use_redc)
	mpz_redc (xx, xx, xx, mod, invm);
      else
	{
	  mpz_mul (xx, xx, xx);
	  mpz_mod (xx, xx, mod);
	}
    }

#ifdef POWM_DEBUG
  printf ("x^"); mpz_out_str (0, 10, exp);
  printf ("*2^%u mod m = ", n * BITS_PER_MP_LIMB); mpz_out_str (0, 10, xx);
  putchar ('\n');
#endif

  while (i > 0 || sh > 0)
    {
      c = ep[i];
      sh -= k;
      l = k;				/* number of bits treated */
      if (sh < 0)
	{
	  if (i > 0)
	    {
	      i--;
	      c = (c << (-sh)) | (ep[i] >> (BITS_PER_MP_LIMB + sh));
	      sh += BITS_PER_MP_LIMB;
	    }
	  else
	    {
	      l += sh;			/* may be less bits than k here */
	      c = c & ((1<<l) - 1);
	    }
	}
      else
	c = c >> sh;
      c = c & mask;

      /* this while loop implements the sliding window improvement */
      while ((c & (1 << (k - 1))) == 0 && (i > 0 || sh > 0))
	{
	  if (use_redc) mpz_redc (xx, xx, xx, mod, invm);
	  else
	    {
	      mpz_mul (xx, xx, xx);
	      mpz_mod (xx, xx, mod);
	    }
	  if (sh)
	    {
	      sh--;
	      c = (c<<1) + ((ep[i]>>sh) & 1);
	    }
	  else
	    {
	      i--;
	      sh = BITS_PER_MP_LIMB - 1;
	      c = (c<<1) + (ep[i]>>sh);
	    }
	}

#ifdef POWM_DEBUG
      printf ("l=%u c=%lu\n", l, c);
      mpz_mul_2exp (exp, exp, k);
      mpz_add_ui (exp, exp, c);
#endif

      /* now replace xx by xx^(2^k)*x^c */
      if (c != 0)
	{
	  j = 0;
	  while (c % 2 == 0)
	    {
	      j++;
	      c = c >> 1;
	    }
	  /* c0 = c * 2^j, i.e. xx^(2^k)*x^c = (A^(2^(k - j))*c)^(2^j) */
	  l -= j;
	  while (l--)
	    if (use_redc) mpz_redc (xx, xx, xx, mod, invm);
	    else
	      {
		mpz_mul (xx, xx, xx);
		mpz_mod (xx, xx, mod);
	      }
	  if (use_redc)
	    mpz_redc (xx, xx, g[c >> 1], mod, invm);
	  else
	    {
	      mpz_mul (xx, xx, g[c >> 1]);
	      mpz_mod (xx, xx, mod);
	    }
	}
      else
	j = l;				/* case c=0 */
      while (j--)
	{
	  if (use_redc)
	    mpz_redc (xx, xx, xx, mod, invm);
	  else
	    {
	      mpz_mul (xx, xx, xx);
	      mpz_mod (xx, xx, mod);
	    }
	}
#ifdef POWM_DEBUG
      printf ("x^"); mpz_out_str (0, 10, exp);
      printf ("*2^%u mod m = ", n * BITS_PER_MP_LIMB); mpz_out_str (0, 10, xx);
      putchar ('\n');
#endif
    }

  /* now convert back xx to xx/R^n */
  if (use_redc)
    {
      mpz_set_ui (g[0], 1);
      mpz_redc (xx, xx, g[0], mod, invm);
      if (mpz_cmp (xx, mod) >= 0)
	mpz_sub (xx, xx, mod);
    }
  mpz_set (res, xx);

  mpz_clear (xx);
  for (i = 0; i < K / 2; i++)
    mpz_clear (g[i]);
  (*_mp_free_func) (g, K / 2 * sizeof (mpz_t));
}
