/* gmp_randseed_ui (state, seed) -- Set initial seed SEED in random
   state STATE.

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
gmp_randseed_ui (gmp_randstate_t rstate,
		  unsigned long int seed)
#else
gmp_randseed_ui (rstate, seed)
     gmp_randstate_t rstate;
     mpz_t seed;
#endif
{
  mpz_set_ui (rstate->seed, seed);
}
