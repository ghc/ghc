/* mpz_urandomm (rop, state, n) -- Generate a uniform pseudorandom
   integer in the range 0 to N-1, using STATE as the random state
   previously initialized by a call to gmp_randinit().

Copyright (C) 2000  Free Software Foundation, Inc.

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

void
#if __STDC__
mpz_urandomm (mpz_t rop, gmp_randstate_t rstate, mpz_t n)
#else
mpz_urandomm (rop, rstate, n)
     mpz_t rop;
     gmp_randstate_t rstate;
     mpz_t n;
#endif
{
  mpz_t t, p, m;
  mp_ptr tp;
  mp_size_t nbits, size;
  int count;
  TMP_DECL (marker);

  TMP_MARK (marker);

  /* FIXME: Should check for n == 0 and report error */

  size = SIZ (n);
  count_leading_zeros (count, PTR (n)[size - 1]);
  nbits = size * BITS_PER_MP_LIMB - count;

  /* Allocate enough for any mpz function called since a realloc of
     these will fail.  */
  MPZ_TMP_INIT (t, size);
  MPZ_TMP_INIT (m, size + 1);
  MPZ_TMP_INIT (p, size + 1);

  /* Let m = highest possible random number plus 1.  */
  mpz_set_ui (m, 0);
  mpz_setbit (m, nbits);

  /* Let p = floor(m / n) * n.  */
  mpz_fdiv_q (p, m, n);
  mpz_mul (p, p, n);

  tp = PTR (t);
  do
    {
      _gmp_rand (tp, rstate, nbits);
      MPN_NORMALIZE (tp, size);	/* FIXME: Really necessary?  */
      SIZ (t) = size;
    }
  while (mpz_cmp (t, p) >= 0);

  mpz_mod (rop, t, n);

  TMP_FREE (marker);
}
