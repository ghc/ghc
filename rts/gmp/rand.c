/* gmp_randinit (state, algorithm, ...) -- Initialize a random state.

Copyright (C) 1999, 2000  Free Software Foundation, Inc.

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

#include <stdio.h> /* for NULL */
#if __STDC__
# include <stdarg.h>
#else
# include <varargs.h>
#endif

#include "gmp.h"
#include "gmp-impl.h"

/* Array of CL-schemes, ordered in increasing order of the first
   member (the 'm2exp' value).  The end of the array is indicated with
   an entry containing all zeros.  */

/* All multipliers are in the range 0.01*m and 0.99*m, and are
congruent to 5 (mod 8).
They all pass the spectral test with Vt >= 2^(30/t) and merit >= 1.
(Up to and including 196 bits, merit is >= 3.)  */

struct __gmp_rand_lc_scheme_struct
{
  unsigned long int m2exp;	/* Modulus is 2 ^ m2exp. */
  char *astr;			/* Multiplier in string form. */
  unsigned long int c;		/* Adder. */
};

struct __gmp_rand_lc_scheme_struct __gmp_rand_lc_scheme[] =
{
  {32, "43840821", 	     1},
  {33, "85943917", 	     1},
  {34, "171799469", 	     1},
  {35, "343825285", 	     1},
  {36, "687285701", 	     1},
  {37, "1374564613", 	     1},
  {38, "2749193437", 	     1},
  {39, "5497652029", 	     1},
  {40, "10995212661", 	     1},
  {56, "47988680294711517",  1},
  {64, "13469374875402548381", 1},
  {100, "203786806069096950756900463357", 1},	
  {128, "96573135900076068624591706046897650309", 1},
  {156, "43051576988660538262511726153887323360449035333", 1},
  {196, "1611627857640767981443524165616850972435303571524033586421", 1},
  {200, "491824250216153841876046962368396460896019632211283945747141", 1},
  {256, "79336254595106925775099152154558630917988041692672147726148065355845551082677", 1},
  {0, NULL, 0}			/* End of array. */
};

void
#if __STDC__
gmp_randinit (gmp_randstate_t rstate,
	      gmp_randalg_t alg,
	      ...)
#else
gmp_randinit (va_alist)
     va_dcl
#endif
{
  va_list ap;
#if __STDC__
#else
  __gmp_randstate_struct *rstate;
  gmp_randalg_t alg;
#endif

#if __STDC__
  va_start (ap, alg);
#else
  va_start (ap);

  rstate = va_arg (ap, __gmp_randstate_struct *);
  alg = va_arg (ap, gmp_randalg_t);
#endif

  switch (alg)
    {
    case GMP_RAND_ALG_LC:	/* Linear congruential.  */
      {
	unsigned long int size;
	struct __gmp_rand_lc_scheme_struct *sp;
	mpz_t a;

	size = va_arg (ap, unsigned long int);

	/* Pick a scheme.  */
	for (sp = __gmp_rand_lc_scheme; sp->m2exp != 0; sp++)
	  if (sp->m2exp / 2 >= size)
	    break;

	if (sp->m2exp == 0)	/* Nothing big enough found.  */
	  {
	    gmp_errno |= GMP_ERROR_INVALID_ARGUMENT;
	    return;
	  }

	/* Install scheme.  */
	mpz_init_set_str (a, sp->astr, 0);
	gmp_randinit_lc_2exp (rstate, a, sp->c, sp->m2exp);
	mpz_clear (a);
	break;
      }

#if 0
    case GMP_RAND_ALG_BBS:	/* Blum, Blum, and Shub. */
      {				
	mpz_t p, q;
	mpz_t ztmp;

	/* FIXME: Generate p and q.  They must be ``large'' primes,
           congruent to 3 mod 4.  Should we ensure that they meet some
           of the criterias for being ``hard primes''?*/

	/* These are around 128 bits. */
	mpz_init_set_str (p, "148028650191182616877187862194899201391", 10); 
	mpz_init_set_str (q, "315270837425234199477225845240496832591", 10);
	
	/* Allocate algorithm specific data. */
	rstate->data.bbs = (__gmp_rand_data_bbs *)
	  (*_mp_allocate_func) (sizeof (__gmp_rand_data_bbs));

	mpz_init (rstate->data.bbs->bi); /* The Blum integer. */
	mpz_mul (rstate->data.bbs->bi, p, q);

	/* Find a seed, x, with gcd (x, bi) == 1. */
	mpz_init (ztmp);
	while (1)
	  {
	    mpz_gcd (ztmp, seed, rstate->data.bbs->bi);
	    if (!mpz_cmp_ui (ztmp, 1))
	      break;
	    mpz_add_ui (seed, seed, 1);
	  }

	rstate->alg = alg;
	rstate->size = size;		/* FIXME: Remove. */
	mpz_set (rstate->seed, seed);

	mpz_clear (p);
	mpz_clear (q);
	mpz_clear (ztmp);
	break;
      }
#endif /* 0 */

    default:			/* Bad choice. */
      gmp_errno |= GMP_ERROR_UNSUPPORTED_ARGUMENT;
    }

  va_end (ap);
}
