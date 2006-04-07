/* mpz_nextprime(p,t) - compute the next prime > t and store that in p.

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

void
#if __STDC__
mpz_nextprime (mpz_ptr p, mpz_srcptr t)
#else
mpz_nextprime (p, t)
     mpz_ptr    p;
     mpz_srcptr t;
#endif
{
  mpz_add_ui (p, t, 1L);
  while (! mpz_probab_prime_p (p, 5))
    mpz_add_ui (p, p, 1L);
}

#if 0
/* This code is not yet tested.  Will be enabled in 3.1. */

status unsigned short primes[] =
{
3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,
101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,
191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,
281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,
389,397,401,409,419,421,431,433,439,443,449,457,461,463,467,479,487,
491,499,503,509,521,523,541,547,557,563,569,571,577,587,593,599,601,
607,613,617,619,631,641,643,647,653,659,661,673,677,683,691,701,709,
719,727,733,739,743,751,757,761,769,773,787,797,809,811,821,823,827,
829,839,853,857,859,863,877,881,883,887,907,911,919,929,937,941,947,
953,967,971,977,983,991,997
};

#define NUMBER_OF_PRIMES 167

void
#if __STDC__
mpz_nextprime (mpz_ptr p, mpz_srcptr n)
#else
mpz_nextprime (p, n)
     mpz_ptr p;
     mpz_srcptr n;
#endif
{
  mpz_t tmp;
  unsigned short *moduli;
  unsigned long difference;
  int i;
  int composite;

  /* First handle tiny numbers */
  if (mpz_cmp_ui (n, 2) < 0)
    {
      mpz_set_ui (p, 2);
      return;
    }
  mpz_add_ui (p, n, 1);
  mpz_setbit (p, 0);

  if (mpz_cmp_ui (p, 7) <= 0)
    return;

  prime_limit = NUMBER_OF_PRIMES - 1;
  if (mpz_cmp_ui (p, primes[prime_limit]) <= 0)
    /* Just use first three entries (3,5,7) of table for small numbers */
    prime_limit = 3;
  if (prime_limit)
    {
      /* Compute residues modulo small odd primes */
      moduli = (unsigned short *) TMP_ALLOC (prime_limit * sizeof moduli[0]);
      for (i = 0; i < prime_limit; i++)
	moduli[i] = mpz_fdiv_ui (p, primes[i]);
    }
  for (difference = 0; ; difference += 2)
    {
      composite = 0;

      /* First check residues */
      for (i = 0; i < prime_limit; i++)
	{
	  int acc, pr;
	  composite |= (moduli[i] == 0);
	  acc = moduli[i] + 2;
	  pr = primes[i];
	  moduli[i] = acc >= pr ? acc - pr : acc;
	}
      if (composite)
	continue;

      mpz_add_ui (p, p, difference);
      difference = 0;

      /* Miller-Rabin test */
      if (mpz_millerrabin (p, 2))
	break;
    }
}
#endif
