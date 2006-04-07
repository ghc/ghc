/* mpz_rrandomb -- Generate a positive random mpz_t of specified bit size, with
   long runs of consecutive ones and zeros in the binary representation.
   Meant for testing of other MP routines.

Copyright (C) 2000 Free Software Foundation, Inc.

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

static void gmp_rrandomb _PROTO ((mp_ptr rp, gmp_randstate_t rstate, unsigned long int nbits));

void
#if __STDC__
mpz_rrandomb (mpz_ptr x, gmp_randstate_t rstate, unsigned long int nbits)
#else
mpz_rrandomb (x, rstate, nbits)
     mpz_ptr x;
     gmp_randstate_t rstate;
     unsigned long int nbits;
#endif
{
  mp_size_t nl = 0;

  if (nbits != 0)
    {
      mp_ptr xp;
      nl = (nbits + BITS_PER_MP_LIMB - 1) / BITS_PER_MP_LIMB;
      if (x->_mp_alloc < nl)
	_mpz_realloc (x, nl);

      xp = PTR(x);
      gmp_rrandomb (xp, rstate, nbits);
      MPN_NORMALIZE (xp, nl);
    }

  SIZ(x) = nl;
}

#define BITS_PER_CHUNK 4

static void
#if __STDC__
gmp_rrandomb (mp_ptr rp, gmp_randstate_t rstate, unsigned long int nbits)
#else
gmp_rrandomb (rp, rstate, nbits)
     mp_ptr rp;
     gmp_randstate_t rstate;
     unsigned long int nbits;
#endif
{
  int nb;
  int bit_pos;
  mp_size_t limb_pos;
  mp_limb_t ran, ranm;
  mp_limb_t acc;
  mp_size_t n;

  bit_pos = nbits % BITS_PER_MP_LIMB;
  limb_pos = nbits / BITS_PER_MP_LIMB;
  if (bit_pos == 0)
    {
      bit_pos = BITS_PER_MP_LIMB;
      limb_pos--;
    }

  acc = 0;
  while (limb_pos >= 0)
    {
      _gmp_rand (&ranm, rstate, BITS_PER_CHUNK + 1);
      ran = ranm;
      nb = (ran >> 1) + 1;
      if ((ran & 1) != 0)
	{
	  /* Generate a string of ones.  */
	  if (nb > bit_pos)
	    {
	      rp[limb_pos--] = acc | ((((mp_limb_t) 1) << bit_pos) - 1);
	      bit_pos += BITS_PER_MP_LIMB;
	      bit_pos -= nb;
	      acc = (~(mp_limb_t) 0) << bit_pos;
	    }
	  else
	    {
	      bit_pos -= nb;
	      acc |= ((((mp_limb_t) 1) << nb) - 1) << bit_pos;
	    }
	}
      else
	{
	  /* Generate a string of zeroes.  */
	  if (nb > bit_pos)
	    {
	      rp[limb_pos--] = acc;
	      acc = 0;
	      bit_pos += BITS_PER_MP_LIMB;
	    }
	  bit_pos -= nb;
	}
    }
}
