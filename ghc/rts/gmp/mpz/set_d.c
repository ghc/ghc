/* mpz_set_d(integer, val) -- Assign INTEGER with a double value VAL.

Copyright (C) 1995, 1996, 2000 Free Software Foundation, Inc.

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

void
#if __STDC__
mpz_set_d (mpz_ptr r, double d)
#else
mpz_set_d (r, d)
     mpz_ptr r;
     double d;
#endif
{
  int negative;
  mp_limb_t tp[3];
  mp_ptr rp;
  mp_size_t rn;

  negative = d < 0;
  d = ABS (d);

  /* Handle small arguments quickly.  */
  if (d < MP_BASE_AS_DOUBLE)
    {
      mp_limb_t tmp;
      tmp = d;
      PTR(r)[0] = tmp;
      SIZ(r) = negative ? -(tmp != 0) : (tmp != 0);
      return;
    }

  rn = __gmp_extract_double (tp, d);

  if (ALLOC(r) < rn)
    _mpz_realloc (r, rn);

  rp = PTR (r);

#if BITS_PER_MP_LIMB == 32
  switch (rn)
    {
    default:
      MPN_ZERO (rp, rn - 3);
      rp += rn - 3;
      /* fall through */
    case 3:
      rp[2] = tp[2];
      rp[1] = tp[1];
      rp[0] = tp[0];
      break;
    case 2:
      rp[1] = tp[2];
      rp[0] = tp[1];
      break;
    case 1:
      /* handled in "small aguments" case above */
      abort ();
    }
#else
  switch (rn)
    {
    default:
      MPN_ZERO (rp, rn - 2);
      rp += rn - 2;
      /* fall through */
    case 2:
      rp[1] = tp[1], rp[0] = tp[0];
      break;
    case 1:
      /* handled in "small aguments" case above */
      abort ();
    }
#endif

  SIZ(r) = negative ? -rn : rn;
}
