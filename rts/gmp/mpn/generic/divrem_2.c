/* mpn_divrem_2 -- Divide natural numbers, producing both remainder and
   quotient.  The divisor is two limbs.

   THIS FILE CONTAINS INTERNAL FUNCTIONS WITH MUTABLE INTERFACES.  IT IS
   ONLY SAFE TO REACH THEM THROUGH DOCUMENTED INTERFACES.  IN FACT, IT IS
   ALMOST GUARANTEED THAT THEY'LL CHANGE OR DISAPPEAR IN A FUTURE GNU MP
   RELEASE.


Copyright (C) 1993, 1994, 1995, 1996, 1999, 2000 Free Software Foundation,
Inc.

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

/* Divide num (NP/NSIZE) by den (DP/2) and write
   the NSIZE-2 least significant quotient limbs at QP
   and the 2 long remainder at NP.  If QEXTRA_LIMBS is
   non-zero, generate that many fraction bits and append them after the
   other quotient limbs.
   Return the most significant limb of the quotient, this is always 0 or 1.

   Preconditions:
   0. NSIZE >= 2.
   1. The most significant bit of the divisor must be set.
   2. QP must either not overlap with the input operands at all, or
      QP + 2 >= NP must hold true.  (This means that it's
      possible to put the quotient in the high part of NUM, right after the
      remainder in NUM.
   3. NSIZE >= 2, even if QEXTRA_LIMBS is non-zero.  */

mp_limb_t
#if __STDC__
mpn_divrem_2 (mp_ptr qp, mp_size_t qxn,
	      mp_ptr np, mp_size_t nsize,
	      mp_srcptr dp)
#else
mpn_divrem_2 (qp, qxn, np, nsize, dp)
     mp_ptr qp;
     mp_size_t qxn;
     mp_ptr np;
     mp_size_t nsize;
     mp_srcptr dp;
#endif
{
  mp_limb_t most_significant_q_limb = 0;
  mp_size_t i;
  mp_limb_t n1, n0, n2;
  mp_limb_t d1, d0;
  mp_limb_t d1inv;
  int have_preinv;

  np += nsize - 2;
  d1 = dp[1];
  d0 = dp[0];
  n1 = np[1];
  n0 = np[0];

  if (n1 >= d1 && (n1 > d1 || n0 >= d0))
    {
      sub_ddmmss (n1, n0, n1, n0, d1, d0);
      most_significant_q_limb = 1;
    }

  /* If multiplication is much faster than division, preinvert the most 
     significant divisor limb before entering the loop.  */
  if (UDIV_TIME > 2 * UMUL_TIME + 6)
    {
      have_preinv = 0;
      if ((UDIV_TIME - (2 * UMUL_TIME + 6)) * (nsize - 2) > UDIV_TIME)
	{
	  invert_limb (d1inv, d1);
	  have_preinv = 1;
	}
    }

  for (i = qxn + nsize - 2 - 1; i >= 0; i--)
    {
      mp_limb_t q;
      mp_limb_t r;

      if (i >= qxn)
	np--;
      else
	np[0] = 0;

      if (n1 == d1)
	{
	  /* Q should be either 111..111 or 111..110.  Need special treatment
	     of this rare case as normal division would give overflow.  */
	  q = ~(mp_limb_t) 0;

	  r = n0 + d1;
	  if (r < d1)	/* Carry in the addition? */
	    {
	      add_ssaaaa (n1, n0, r - d0, np[0], 0, d0);
	      qp[i] = q;
	      continue;
	    }
	  n1 = d0 - (d0 != 0);
	  n0 = -d0;
	}
      else
	{
	  if (UDIV_TIME > 2 * UMUL_TIME + 6 && have_preinv)
	    udiv_qrnnd_preinv (q, r, n1, n0, d1, d1inv);
	  else
	    udiv_qrnnd (q, r, n1, n0, d1);
	  umul_ppmm (n1, n0, d0, q);
	}

      n2 = np[0];

    q_test:
      if (n1 > r || (n1 == r && n0 > n2))
	{
	  /* The estimated Q was too large.  */
	  q--;

	  sub_ddmmss (n1, n0, n1, n0, 0, d0);
	  r += d1;
	  if (r >= d1)	/* If not carry, test Q again.  */
	    goto q_test;
	}

      qp[i] = q;
      sub_ddmmss (n1, n0, r, n2, n1, n0);
    }
  np[1] = n1;
  np[0] = n0;

  return most_significant_q_limb;
}
