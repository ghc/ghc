/* mpn_mod_1_rshift -- mpn remainder under hypothetical right shift.

   THE FUNCTION IN THIS FILE IS FOR INTERNAL USE AND HAS A MUTABLE
   INTERFACE.  IT IS ONLY SAFE TO REACH IT THROUGH DOCUMENTED INTERFACES.
   IT'S ALMOST GUARANTEED THAT IT'LL CHANGE OR DISAPPEAR IN A FUTURE GNU MP
   RELEASE. */

/*
Copyright (C) 1999, 2000 Free Software Foundation, Inc.

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
MA 02111-1307, USA.  */

#include "gmp.h"
#include "gmp-impl.h"
#include "longlong.h"


/* When testing on a CPU with UDIV_NEEDS_NORMALIZATION equal to 0, it can be
   changed to 1 temporarily to test the code under that case too. */
#if 0
#undef UDIV_NEEDS_NORMALIZATION
#define UDIV_NEEDS_NORMALIZATION 1
#endif


/* Calculate the remainder "(ptr,size >> shift) % divisor".  Note ptr,size
   is unchanged, the shift is only for its effect on the remainder.
   The shift doesn't even need to be considered until the last limb.

   This function has the normal size!=0 restriction, unlike the basic
   mpn_mod_1. */

mp_limb_t
#if __STDC__
mpn_mod_1_rshift (mp_srcptr ptr, mp_size_t size, unsigned shift,
                  mp_limb_t divisor)
#else
mpn_mod_1_rshift (ptr, size, shift, divisor)
     mp_srcptr ptr;
     mp_size_t size;
     unsigned  shift;
     mp_limb_t divisor;
#endif
{
  mp_limb_t  quot, rem;

  ASSERT (shift >= 1);
  ASSERT (shift < BITS_PER_MP_LIMB);
  ASSERT (size >= 1);

  if (size == 1)
    return (ptr[0] >> shift) % divisor;

#if UDIV_NEEDS_NORMALIZATION 
  {
    int  norm;
    int  delta;

    count_leading_zeros (norm, divisor);
    divisor <<= norm;

    delta = shift - norm;
    if (delta == 0)
      return mpn_mod_1 (ptr, size, divisor) >> norm;

    if (delta > 0)
      {
        rem = mpn_mod_1 (ptr+1, size-1, divisor);
        udiv_qrnnd (quot, rem,
                    rem >> delta,
                    (rem << (BITS_PER_MP_LIMB-delta)) | (ptr[0] >> delta),
                    divisor);
        return rem >> norm;
      }
    else
      {
        rem = mpn_mod_1 (ptr, size, divisor);
        udiv_qrnnd (quot, rem,
                    rem >> (BITS_PER_MP_LIMB+delta),
                    rem << -delta,
                    divisor);
        return rem >> norm;
      }
  }

#else /* !UDIV_NEEDS_NORMALIZATION */

  rem = mpn_mod_1 (ptr+1, size-1, divisor);
  udiv_qrnnd (quot, rem,
              rem >> shift,
              (rem << (BITS_PER_MP_LIMB-shift)) | (ptr[0] >> shift),
              divisor);
  return rem;

#endif
}
