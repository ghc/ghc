/* mpz_tstbit -- test a specified bit.  Simulate 2's complement representation.

Copyright (C) 1997 Free Software Foundation, Inc.

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

int
#if __STDC__
mpz_tstbit (mpz_srcptr d, unsigned long int bit_index)
#else
mpz_tstbit (d, bit_index)
     mpz_srcptr d;
     unsigned long int bit_index;
#endif
{
  mp_size_t dsize = d->_mp_size;
  mp_ptr dp = d->_mp_d;
  mp_size_t limb_index;

  limb_index = bit_index / BITS_PER_MP_LIMB;
  if (dsize >= 0)
    {
      if (limb_index < dsize)
	return (dp[limb_index] >> (bit_index % BITS_PER_MP_LIMB)) & 1;
      else
	/* Testing a bit outside of a positive number.  */
	return 0;
    }
  else
    {
      mp_size_t zero_bound;

      dsize = -dsize;

      /* Locate the least significant non-zero limb.  */
      for (zero_bound = 0; dp[zero_bound] == 0; zero_bound++)
	;

      if (limb_index > zero_bound)
	{
	  if (limb_index < dsize)
	    return (~dp[limb_index] >> (bit_index % BITS_PER_MP_LIMB)) & 1;
	  else
	    /* Testing a bit outside of a negative number.  */
	    return 1;
	}
      else if (limb_index == zero_bound)
	return (-dp[limb_index] >> (bit_index % BITS_PER_MP_LIMB)) & 1;
      else
	return 0;
    }
}
