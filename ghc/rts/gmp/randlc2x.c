/* gmp_randinit_lc_2exp (state, a, c, m2exp) -- Initialize random
   state STATE for a linear congruential generator with multiplier A,
   adder C, and modulus 2 ^ M2EXP.

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

void
#if __STDC__
gmp_randinit_lc_2exp (gmp_randstate_t rstate,
		      mpz_t a,
		      unsigned long int c,
		      unsigned long int m2exp)
#else
gmp_randinit_lc_2exp (rstate, a, c, m2exp)
     gmp_randstate_t rstate;
     mpz_t a;
     unsigned long int c;
     unsigned long int m2exp;
#endif
{
  mpz_init_set_ui (rstate->seed, 1);
  _mpz_realloc (rstate->seed, m2exp / BITS_PER_MP_LIMB
		+ (m2exp % BITS_PER_MP_LIMB != 0));

  /* Allocate algorithm specific data. */
  rstate->algdata.lc = (__gmp_randata_lc *)
    (*_mp_allocate_func) (sizeof (__gmp_randata_lc));

  mpz_init_set (rstate->algdata.lc->a, a);
  rstate->algdata.lc->c = c;

  /* Cover weird case where m2exp is 0, which means that m is used
     instead of m2exp.  */
  if (m2exp == 0)
    mpz_init_set_ui (rstate->algdata.lc->m, 0);
  rstate->algdata.lc->m2exp = m2exp;

  rstate->alg = GMP_RAND_ALG_LC;
}
