/* double mpz_get_d (mpz_t src) -- Return the double approximation to SRC.

Copyright (C) 1996, 1997, 2000 Free Software Foundation, Inc.

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
#include "longlong.h"


static int
#if __STDC__
mpn_zero_p (mp_ptr p, mp_size_t n)
#else
mpn_zero_p (p, n)
     mp_ptr p;
     mp_size_t n;
#endif
{
  mp_size_t i;

  for (i = 0; i < n; i++)
    {
      if (p[i] != 0)
	return 0;
    }

  return 1;
}


double
#if __STDC__
mpz_get_d (mpz_srcptr src)
#else
mpz_get_d (src)
     mpz_srcptr src;
#endif
{
  double res;
  mp_size_t size;
  int negative;
  mp_ptr qp;
  mp_limb_t hz, lz;
  int cnt;

  size = SIZ(src);
  if (size == 0)
    return 0.0;

  negative = size < 0;
  size = ABS (size);
  qp = PTR(src);

  if (size == 1)
    {
      res = qp[size - 1];
    }
  else if (size == 2)
    {
      res = MP_BASE_AS_DOUBLE * qp[size - 1] + qp[size - 2];
    }
  else
    {
      count_leading_zeros (cnt, qp[size - 1]);

#if BITS_PER_MP_LIMB == 32
      if (cnt == 0)
	{
	  hz = qp[size - 1];
	  lz = qp[size - 2];
	}
      else
	{
	  hz = (qp[size - 1] << cnt) | (qp[size - 2] >> BITS_PER_MP_LIMB - cnt);
	  lz = (qp[size - 2] << cnt) | (qp[size - 3] >> BITS_PER_MP_LIMB - cnt);
	}
#if _GMP_IEEE_FLOATS
      /* Take bits from less significant limbs, but only if they may affect
	 the result.  */
      if ((lz & 0x7ff) == 0x400)
	{
	  if (cnt != 0)
	    lz += ((qp[size - 3] << cnt) != 0 || ! mpn_zero_p (qp, size - 3));
	  else
	    lz += (! mpn_zero_p (qp, size - 2));
	}
#endif
      res = MP_BASE_AS_DOUBLE * hz + lz;
      res = __gmp_scale2 (res, (size - 2) * BITS_PER_MP_LIMB - cnt);
#endif
#if BITS_PER_MP_LIMB == 64
      if (cnt == 0)
	hz = qp[size - 1];
      else
	hz = (qp[size - 1] << cnt) | (qp[size - 2] >> BITS_PER_MP_LIMB - cnt);
#if _GMP_IEEE_FLOATS
      if ((hz & 0x7ff) == 0x400)
	{
	  if (cnt != 0)
	    hz += ((qp[size - 2] << cnt) != 0 || ! mpn_zero_p (qp, size - 2));
	  else
	    hz += (! mpn_zero_p (qp, size - 1));
	}
#endif
      res = hz;
      res = __gmp_scale2 (res, (size - 1) * BITS_PER_MP_LIMB - cnt);
#endif
    }

  return negative ? -res : res;
}
