/* mpn_divexact_by3 -- mpn division by 3, expecting no remainder. */

/*
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
MA 02111-1307, USA.
*/


#include "gmp.h"
#include "gmp-impl.h"


/* Multiplicative inverse of 3, modulo 2^BITS_PER_MP_LIMB.
   0xAAAAAAAB for 32 bits, 0xAAAAAAAAAAAAAAAB for 64 bits. */
#define INVERSE_3      ((MP_LIMB_T_MAX / 3) * 2 + 1)


/* The "c += ..."s are adding the high limb of 3*l to c.  That high limb
   will be 0, 1 or 2.  Doing two separate "+="s seems to turn out better
   code on gcc (as of 2.95.2 at least).

   When a subtraction of a 0,1,2 carry value causes a borrow, that leaves a
   limb value of either 0xFF...FF or 0xFF...FE and the multiply by INVERSE_3
   gives 0x55...55 or 0xAA...AA respectively, producing a further borrow of
   only 0 or 1 respectively.  Hence the carry out of each stage and for the
   return value is always only 0, 1 or 2.  */

mp_limb_t
#if __STDC__
mpn_divexact_by3c (mp_ptr dst, mp_srcptr src, mp_size_t size, mp_limb_t c)
#else
mpn_divexact_by3c (dst, src, size, c)
     mp_ptr    dst;
     mp_srcptr src;
     mp_size_t size;
     mp_limb_t c;
#endif
{
  mp_size_t  i;

  ASSERT (size >= 1);

  i = 0;
  do
    {
      mp_limb_t  l, s;

      s = src[i];
      l = s - c;
      c = (l > s);

      l *= INVERSE_3;
      dst[i] = l;

      c += (l > MP_LIMB_T_MAX/3);
      c += (l > (MP_LIMB_T_MAX/3)*2);
    }
  while (++i < size);

  return c;
}
