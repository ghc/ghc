/* mpz_remove -- divide out a factor and return its multiplicity.

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

#include "gmp.h"
#include "gmp-impl.h"

unsigned long int
#if __STDC__
mpz_remove (mpz_ptr dest, mpz_srcptr src, mpz_srcptr f)
#else
mpz_remove (dest, src, f)
     mpz_ptr dest;
     mpz_srcptr src;
     mpz_srcptr f;
#endif
{
  mpz_t fpow[40];		/* inexhaustible...until year 2020 or so */
  mpz_t x, rem;
  unsigned long int pwr;
  int p;

  if (mpz_cmp_ui (f, 1) <= 0 || mpz_sgn (src) == 0)
    DIVIDE_BY_ZERO;
  if (mpz_cmp_ui (f, 2) == 0)
    {
      unsigned long int s0;
      s0 = mpz_scan1 (src, 0);
      mpz_div_2exp (dest, src, s0);
      return s0;
    }

  /* We could perhaps compute mpz_scan1(src,0)/mpz_scan1(f,0).  It is an
     upper bound of the result we're seeking.  We could also shift down the
     operands so that they become odd, to make intermediate values smaller.  */

  mpz_init (rem);
  mpz_init (x);

  pwr = 0;
  mpz_init (fpow[0]);
  mpz_set (fpow[0], f);
  mpz_set (dest, src);

  /* Divide by f, f^2, ..., f^(2^k) until we get a remainder for f^(2^k).  */
  for (p = 0;; p++)
    {
      mpz_tdiv_qr (x, rem, dest, fpow[p]);
      if (SIZ (rem) != 0)
	break;
      mpz_init (fpow[p + 1]);
      mpz_mul (fpow[p + 1], fpow[p], fpow[p]);
      mpz_set (dest, x);
    }

  pwr = (1 << p) - 1;

  mpz_clear (fpow[p]);

  /* Divide by f^(2^(k-1)), f^(2^(k-2)), ..., f for all divisors that give a
     zero remainder.  */
  while (--p >= 0)
    {
      mpz_tdiv_qr (x, rem, dest, fpow[p]);
      if (SIZ (rem) == 0)
	{
	  pwr += 1 << p;
	  mpz_set (dest, x);
	}
      mpz_clear (fpow[p]);
    }

  mpz_clear (x);
  mpz_clear (rem);
  return pwr;
}
