/* mpn_mul_basecase -- Internal routine to multiply two natural numbers
   of length m and n.

   THIS IS AN INTERNAL FUNCTION WITH A MUTABLE INTERFACE.  IT IS ONLY
   SAFE TO REACH THIS FUNCTION THROUGH DOCUMENTED INTERFACES.


Copyright (C) 1991, 1992, 1993, 1994, 1996, 1997, 2000 Free Software
Foundation, Inc.

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

/* Handle simple cases with traditional multiplication.

   This is the most critical code of multiplication.  All multiplies rely on
   this, both small and huge.  Small ones arrive here immediately, huge ones
   arrive here as this is the base case for Karatsuba's recursive algorithm. */

void
#if __STDC__
mpn_mul_basecase (mp_ptr prodp,
		     mp_srcptr up, mp_size_t usize,
		     mp_srcptr vp, mp_size_t vsize)
#else
mpn_mul_basecase (prodp, up, usize, vp, vsize)
     mp_ptr prodp;
     mp_srcptr up;
     mp_size_t usize;
     mp_srcptr vp;
     mp_size_t vsize;
#endif
{
  /* We first multiply by the low order one or two limbs, as the result can
     be stored, not added, to PROD.  We also avoid a loop for zeroing this
     way.  */
#if HAVE_NATIVE_mpn_mul_2
  if (vsize >= 2)
    {
      prodp[usize + 1] = mpn_mul_2 (prodp, up, usize, vp[0], vp[1]);
      prodp += 2, vp += 2, vsize -= 2;
    }
  else
    {
      prodp[usize] = mpn_mul_1 (prodp, up, usize, vp[0]);
      return;
    }
#else
  prodp[usize] = mpn_mul_1 (prodp, up, usize, vp[0]);
  prodp += 1, vp += 1, vsize -= 1;
#endif

#if HAVE_NATIVE_mpn_addmul_2
  while (vsize >= 2)
    {
      prodp[usize + 1] = mpn_addmul_2 (prodp, up, usize, vp[0], vp[1]);
      prodp += 2, vp += 2, vsize -= 2;
    }
  if (vsize != 0)
    prodp[usize] = mpn_addmul_1 (prodp, up, usize, vp[0]);
#else
  /* For each iteration in the loop, multiply U with one limb from V, and
     add the result to PROD.  */
  while (vsize != 0)
    {
      prodp[usize] = mpn_addmul_1 (prodp, up, usize, vp[0]);
      prodp += 1, vp += 1, vsize -= 1;
    }
#endif
}
