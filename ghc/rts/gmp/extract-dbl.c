/* __gmp_extract_double -- convert from double to array of mp_limb_t.

Copyright (C) 1996, 1999, 2000 Free Software Foundation, Inc.

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

/* Extract a non-negative double in d.  */

int
#if __STDC__
__gmp_extract_double (mp_ptr rp, double d)
#else
__gmp_extract_double (rp, d)
     mp_ptr rp;
     double d;
#endif
{
  long exp;
  unsigned sc;
  mp_limb_t manh, manl;

  /* BUGS

     1. Should handle Inf and NaN in IEEE specific code.
     2. Handle Inf and NaN also in default code, to avoid hangs.
     3. Generalize to handle all BITS_PER_MP_LIMB >= 32.
     4. This lits is incomplete and misspelled.
   */

  if (d == 0.0)
    {
      rp[0] = 0;
      rp[1] = 0;
#if BITS_PER_MP_LIMB == 32
      rp[2] = 0;
#endif
      return 0;
    }

#if _GMP_IEEE_FLOATS
  {
#if defined (__alpha) && __GNUC__ == 2 && __GNUC_MINOR__ == 8
    /* Work around alpha-specific bug in GCC 2.8.x.  */
    volatile
#endif
    union ieee_double_extract x;
    x.d = d;
    exp = x.s.exp;
#if BITS_PER_MP_LIMB == 64
    manl = (((mp_limb_t) 1 << 63)
	    | ((mp_limb_t) x.s.manh << 43) | ((mp_limb_t) x.s.manl << 11));
    if (exp == 0)
      {
	/* Denormalized number.  Don't try to be clever about this,
	   since it is not an important case to make fast.  */
	exp = 1;
	do
	  {
	    manl = manl << 1;
	    exp--;
	  }
	while ((mp_limb_signed_t) manl >= 0);
      }
#else
    manh = ((mp_limb_t) 1 << 31) | (x.s.manh << 11) | (x.s.manl >> 21);
    manl = x.s.manl << 11;
    if (exp == 0)
      {
	/* Denormalized number.  Don't try to be clever about this,
	   since it is not an important case to make fast.  */
	exp = 1;
	do
	  {
	    manh = (manh << 1) | (manl >> 31);
	    manl = manl << 1;
	    exp--;
	  }
	while ((mp_limb_signed_t) manh >= 0);
      }
#endif
    exp -= 1022;		/* Remove IEEE bias.  */
  }
#else
  {
    /* Unknown (or known to be non-IEEE) double format.  */
    exp = 0;
    if (d >= 1.0)
      {
	if (d * 0.5 == d)
	  abort ();

	while (d >= 32768.0)
	  {
	    d *= (1.0 / 65536.0);
	    exp += 16;
	  }
	while (d >= 1.0)
	  {
	    d *= 0.5;
	    exp += 1;
	  }
      }
    else if (d < 0.5)
      {
	while (d < (1.0 / 65536.0))
	  {
	    d *=  65536.0;
	    exp -= 16;
	  }
	while (d < 0.5)
	  {
	    d *= 2.0;
	    exp -= 1;
	  }
      }

    d *= MP_BASE_AS_DOUBLE;
#if BITS_PER_MP_LIMB == 64
    manl = d;
#else
    manh = d;
    manl = (d - manh) * MP_BASE_AS_DOUBLE;
#endif
  }
#endif

  sc = (unsigned) exp % BITS_PER_MP_LIMB;

  /* We add something here to get rounding right.  */
  exp = (exp + 2048) / BITS_PER_MP_LIMB - 2048 / BITS_PER_MP_LIMB + 1;

#if BITS_PER_MP_LIMB == 64
  if (sc != 0)
    {
      rp[1] = manl >> (BITS_PER_MP_LIMB - sc);
      rp[0] = manl << sc;
    }
  else
    {
      rp[1] = manl;
      rp[0] = 0;
      exp--;
    }
#else
  if (sc != 0)
    {
      rp[2] = manh >> (BITS_PER_MP_LIMB - sc);
      rp[1] = (manl >> (BITS_PER_MP_LIMB - sc)) | (manh << sc);
      rp[0] = manl << sc;
    }
  else
    {
      rp[2] = manh;
      rp[1] = manl;
      rp[0] = 0;
      exp--;
    }
#endif

  return exp;
}
