/* mpz_perfect_power_p(arg) -- Return non-zero if ARG is a perfect power,
   zero otherwise.

Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.

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

/*
  We are to determine if c is a perfect power, c = a ^ b.
  Assume c is divisible by 2^n and that codd = c/2^n is odd.
  Assume a is divisible by 2^m and that aodd = a/2^m is odd.
  It is always true that m divides n.

  * If n is prime, either 1) a is 2*aodd and b = n
		       or 2) a = c and b = 1.
    So for n prime, we readily have a solution.
  * If n is factorable into the non-trivial factors p1,p2,...
    Since m divides n, m has a subset of n's factors and b = n / m.

    BUG: Should handle negative numbers, since they can be odd perfect powers.
*/

/* This is a naive approach to recognizing perfect powers.
   Many things can be improved.  In particular, we should use p-adic
   arithmetic for computing possible roots.  */

#include <stdio.h> /* for NULL */
#include "gmp.h"
#include "gmp-impl.h"
#include "longlong.h"

static unsigned long int gcd _PROTO ((unsigned long int a, unsigned long int b));
static int isprime _PROTO ((unsigned long int t));

static const unsigned short primes[] =
{  2,  3,  5,  7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53,
  59, 61, 67, 71, 73, 79, 83, 89, 97,101,103,107,109,113,127,131,
 137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,
 227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,
 313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,
 419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,
 509,521,523,541,547,557,563,569,571,577,587,593,599,601,607,613,
 617,619,631,641,643,647,653,659,661,673,677,683,691,701,709,719,
 727,733,739,743,751,757,761,769,773,787,797,809,811,821,823,827,
 829,839,853,857,859,863,877,881,883,887,907,911,919,929,937,941,
 947,953,967,971,977,983,991,997,0
};
#define SMALLEST_OMITTED_PRIME 1009


int
#if __STDC__
mpz_perfect_power_p (mpz_srcptr u)
#else
mpz_perfect_power_p (u)
     mpz_srcptr u;
#endif
{
  unsigned long int prime;
  unsigned long int n, n2;
  int i;
  unsigned long int rem;
  mpz_t u2, q;
  int exact;
  mp_size_t uns;
  TMP_DECL (marker);

  if (mpz_cmp_ui (u, 1) <= 0)
    return 0;

  n2 = mpz_scan1 (u, 0);
  if (n2 == 1)
    return 0;

  TMP_MARK (marker);

  uns = ABSIZ (u) - n2 / BITS_PER_MP_LIMB;
  MPZ_TMP_INIT (q, uns);
  MPZ_TMP_INIT (u2, uns);

  mpz_tdiv_q_2exp (u2, u, n2);

  if (isprime (n2))
    goto n2prime;

  for (i = 1; primes[i] != 0; i++)
    {
      prime = primes[i];
      rem = mpz_tdiv_ui (u2, prime);
      if (rem == 0)		/* divisable? */
	{
	  rem = mpz_tdiv_q_ui (q, u2, prime * prime);
	  if (rem != 0)
	    {
	      TMP_FREE (marker);
	      return 0;
	    }
	  mpz_swap (q, u2);
	  for (n = 2;;)
	    {
	      rem = mpz_tdiv_q_ui (q, u2, prime);
	      if (rem != 0)
		break;
	      mpz_swap (q, u2);
	      n++;
	    }

	  n2 = gcd (n2, n);
	  if (n2 == 1)
	    {
	      TMP_FREE (marker);
	      return 0;
	    }

	  /* As soon as n2 becomes a prime number, stop factoring.
	     Either we have u=x^n2 or u is not a perfect power.  */
	  if (isprime (n2))
	    goto n2prime;
	}
    }

  if (mpz_cmp_ui (u2, 1) == 0)
    {
      TMP_FREE (marker);
      return 1;
    }

  if (n2 == 0)
    {
      unsigned long int nth;
      /* We did not find any factors above.  We have to consider all values
	 of n.  */
      for (nth = 2;; nth++)
	{
	  if (! isprime (nth))
	    continue;
#if 0
	  exact = mpz_padic_root (q, u2, nth, PTH);
	  if (exact)
#endif
	    exact = mpz_root (q, u2, nth);
	  if (exact)
	    {
	      TMP_FREE (marker);
	      return 1;
	    }
	  if (mpz_cmp_ui (q, SMALLEST_OMITTED_PRIME) < 0)
	    {
	      TMP_FREE (marker);
	      return 0;
	    }
	}
    }
  else
    {
      unsigned long int nth;
      /* We found some factors above.  We just need to consider values of n
	 that divides n2.  */
      for (nth = 2; nth <= n2; nth++)
	{
	  if (! isprime (nth))
	    continue;
	  if (n2 % nth != 0)
	    continue;
#if 0
	  exact = mpz_padic_root (q, u2, nth, PTH);
	  if (exact)
#endif
	    exact = mpz_root (q, u2, nth);
	  if (exact)
	    {
	      TMP_FREE (marker);
	      return 1;
	    }
	  if (mpz_cmp_ui (q, SMALLEST_OMITTED_PRIME) < 0)
	    {
	      TMP_FREE (marker);
	      return 0;
	    }
	}

      TMP_FREE (marker);
      return 0;
    }

n2prime:
  exact = mpz_root (NULL, u2, n2);
  TMP_FREE (marker);
  return exact;
}

static unsigned long int
#if __STDC__
gcd (unsigned long int a, unsigned long int b)
#else
gcd (a, b)
     unsigned long int a, b;
#endif
{
  int an2, bn2, n2;

  if (a == 0)
    return b;
  if (b == 0)
    return a;

  count_trailing_zeros (an2, a);
  a >>= an2;

  count_trailing_zeros (bn2, b);
  b >>= bn2;

  n2 = MIN (an2, bn2);

  while (a != b)
    {
      if (a > b)
	{
	  a -= b;
	  do
	    a >>= 1;
	  while ((a & 1) == 0);
	}
      else /*  b > a.  */
	{
	  b -= a;
	  do
	    b >>= 1;
	  while ((b & 1) == 0);
	}
    }

  return a << n2;
}

static int
#if __STDC__
isprime (unsigned long int t)
#else
isprime (t)
     unsigned long int t;
#endif
{
  unsigned long int q, r, d;

  if (t < 3 || (t & 1) == 0)
    return t == 2;

  for (d = 3, r = 1; r != 0; d += 2)
    {
      q = t / d;
      r = t - q * d;
      if (q < d)
	return 1;
    }
  return 0;
}
