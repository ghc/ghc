/* gmp_randinit_lc (state, a, c, m) -- Initialize a random state for a
   linear congruential generator with multiplier A, adder C, and
   modulus M.

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

#include "gmp.h"
#include "gmp-impl.h"

void
#if __STDC__
gmp_randinit_lc (gmp_randstate_t rstate,
		 mpz_t a,
		 unsigned long int c,
		 mpz_t m)
#else
gmp_randinit_lc (rstate, a, c, m)
     gmp_randstate_t rstate;
     mpz_t a;
     unsigned long int c;
     mpz_t m;
#endif
{
  /* FIXME: Not finished.  We don't handle this in _gmp_rand() yet. */
  abort ();			

  mpz_init_set_ui (rstate->seed, 1);
  _mpz_realloc (rstate->seed, ABSIZ (m));

  /* Allocate algorithm specific data. */
  rstate->algdata.lc = (__gmp_randata_lc *)
    (*_mp_allocate_func) (sizeof (__gmp_randata_lc));

  mpz_init_set (rstate->algdata.lc->a, a);
  rstate->algdata.lc->c = c;
  mpz_init_set (rstate->algdata.lc->m, m);

  rstate->alg = GMP_RAND_ALG_LC;
}
