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
MA 02111-1307, USA.
*/

#include "gmp.h"
#include "gmp-impl.h"
#include "longlong.h"

#define TWO64 18446744073709551616.0
#define TWO63 9223372036854775808.0

mp_limb_t
#if __STDC__
__MPN(udiv_qrnnd) (mp_limb_t n1, mp_limb_t n0, mp_limb_t d, mp_limb_t *r)
#else
__MPN(udiv_qrnnd) (n1, n0, d, r)
     mp_limb_t n1;
     mp_limb_t n0;
     mp_limb_t d;
     mp_limb_t *r;
#endif
{
  mp_limb_t q1, q2, q;
  mp_limb_t p1, p0;
  double di, dq;

  di = 1.0 / d;

  /* Generate upper 53 bits of quotient.  Be careful here; the `double'
     quotient may be rounded to 2^64 which we cannot safely convert back
     to a 64-bit integer.  */
  dq = (TWO64 * (double) n1 + (double) n0) * di;
  if (dq >= TWO64)
    q1 = 0xfffffffffffff800L;
#ifndef __GNUC__
  /* Work around HP compiler bug.  */
  else if (dq > TWO63)
    q1 = (mp_limb_t) (dq - TWO63) + 0x8000000000000000L;
#endif
  else
    q1 = (mp_limb_t) dq;

  /* Multiply back in order to compare the product to the dividend.  */
  umul_ppmm (p1, p0, q1, d);

  /* Was the 53-bit quotient greater that our sought quotient?  Test the
     sign of the partial remainder to find out.  */
  if (n1 < p1 || (n1 == p1 && n0 < p0))
    {
      /* 53-bit quotient too large.  Partial remainder is negative.
	 Compute the absolute value of the remainder in n1,,n0.  */
      n1 = p1 - (n1 + (p0 < n0));
      n0 = p0 - n0;

      /* Now use the partial remainder as new dividend to compute more bits of
	 quotient.  This is an adjustment for the one we got previously.  */
      q2 = (mp_limb_t) ((TWO64 * (double) n1 + (double) n0) * di);
      umul_ppmm (p1, p0, q2, d);

      q = q1 - q2;
      if (n1 < p1 || (n1 == p1 && n0 <= p0))
	{
	  n0 = p0 - n0;
	}
      else
	{
	  n0 = p0 - n0;
	  n0 += d;
	  q--;
	}
    }
  else
    {
      n1 = n1 - (p1 + (n0 < p0));
      n0 = n0 - p0;

      q2 = (mp_limb_t) ((TWO64 * (double) n1 + (double) n0) * di);
      umul_ppmm (p1, p0, q2, d);

      q = q1 + q2;
      if (n1 < p1 || (n1 == p1 && n0 < p0))
	{
	  n0 = n0 - p0;
	  n0 += d;
	  q--;
	}
      else
	{
	  n0 = n0 - p0;
	  if (n0 >= d)
	    {
	      n0 -= d;
	      q++;
	    }
	}
    }

  *r = n0;
  return q;
}
