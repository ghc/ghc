/* __gmp_insert_double -- convert from array of mp_limb_t to double.

Copyright (C) 1996, 1997, 1999, 2000 Free Software Foundation, Inc.

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

#ifdef XDEBUG
#undef _GMP_IEEE_FLOATS
#endif

#ifndef _GMP_IEEE_FLOATS
#define _GMP_IEEE_FLOATS 0
#endif

double
#if __STDC__
__gmp_scale2 (double d, int exp)
#else
__gmp_scale2 (d, exp)
     double d;
     int exp;
#endif
{
#if _GMP_IEEE_FLOATS
  {
#if defined (__alpha) && __GNUC__ == 2 && __GNUC_MINOR__ == 8
    /* Work around alpha-specific bug in GCC 2.8.x.  */
    volatile
#endif
    union ieee_double_extract x;
    x.d = d;
    exp += x.s.exp;
    x.s.exp = exp;
    if (exp >= 2047)
      {
	/* Return +-infinity */
	x.s.exp = 2047;
	x.s.manl = x.s.manh = 0;
      }
    else if (exp < 1)
      {
	x.s.exp = 1;		/* smallest exponent (biased) */
	/* Divide result by 2 until we have scaled it to the right IEEE
	   denormalized number, but stop if it becomes zero.  */
	while (exp < 1 && x.d != 0)
	  {
	    x.d *= 0.5;
	    exp++;
	  }
      }
    return x.d;
  }
#else
  {
    double factor, r;

    factor = 2.0;
    if (exp < 0)
      {
	factor = 0.5;
	exp = -exp;
      }
    r = d;
    if (exp != 0)
      {
	if ((exp & 1) != 0)
	  r *= factor;
	exp >>= 1;
	while (exp != 0)
	  {
	    factor *= factor;
	    if ((exp & 1) != 0)
	      r *= factor;
	    exp >>= 1;
	  }
      }
    return r;
  }
#endif
}
