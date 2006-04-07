/* mpn_sub_n -- Subtract two limb vectors of equal, non-zero length.
   For Cray vector processors.

   Copyright (C) 1996, 2000 Free Software Foundation, Inc.

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

mp_limb_t
mpn_sub_n (c, a, b, n)
     mp_ptr c;
     mp_srcptr a, b;
     mp_size_t n;
{
  mp_size_t i;
  mp_size_t nm1 = n - 1;
  int more_carries = 0;
  int carry_out;

  /* For small operands the non-vector code is faster.  */
  if (n < 16)
    goto sequential;

  if (a == c || b == c)
    {
      TMP_DECL (marker);
      TMP_MARK (marker);
      if (c == a)
	{
	  /* allocate temp space for a */
	  mp_ptr ax = (mp_ptr) TMP_ALLOC (n * BYTES_PER_MP_LIMB);
	  MPN_COPY (ax, a, n);
	  a = (mp_srcptr) ax;
	}
      if (c == b)
	{
	  /* allocate temp space for b */
	  mp_ptr bx = (mp_ptr) TMP_ALLOC (n * BYTES_PER_MP_LIMB);
	  MPN_COPY (bx, b, n);
	  b = (mp_srcptr) bx;
	}
      carry_out = mpn_sub_n (c, a, b, n);
      TMP_FREE (marker);
      return carry_out;
    }

  carry_out = a[nm1] < b[nm1];

#pragma _CRI ivdep			/* Cray PVP systems */
  for (i = nm1; i > 0; i--)
    {
      int cy_in; mp_limb_t t;
      cy_in = a[i - 1] < b[i - 1];
      t = a[i] - b[i];
      more_carries += t < cy_in;
      c[i] = t - cy_in;
    }
  c[0] = a[0] - b[0];

  if (more_carries)
    {
      /* This won't vectorize, but we should come here rarely.  */
      int cy;
    sequential:
      cy = 0;
      for (i = 0; i < n; i++)
	{
	  mp_limb_t ai, ci, t;
	  ai = a[i];
	  t = b[i] + cy;
	  cy = t < cy;
	  ci = ai - t;
	  cy += ci > ai;
	  c[i] = ci;
	}
      carry_out = cy;
    }

  return carry_out;
}
