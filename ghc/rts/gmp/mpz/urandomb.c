/* mpz_urandomb (rop, state, n) -- Generate a uniform pseudorandom
   integer in the range 0 to 2^N - 1, inclusive, using STATE as the
   random state previously initialized by a call to gmp_randinit().

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
mpz_urandomb (mpz_t rop, gmp_randstate_t rstate, unsigned long int nbits)
#else
mpz_urandomb (rop, rstate, nbits)
     mpz_t rop;
     gmp_randstate_t rstate;
     unsigned long int nbits;
#endif
{
  mp_ptr rp;
  mp_size_t size;

  size = (nbits + BITS_PER_MP_LIMB - 1) / BITS_PER_MP_LIMB;
  if (ALLOC (rop) < size)
    _mpz_realloc (rop, size);

  rp = PTR (rop);

  _gmp_rand (rp, rstate, nbits);
  MPN_NORMALIZE (rp, size);
  SIZ (rop) = size;
}
