/* mpn_bz_divrem_n and auxilliary routines.

   THE FUNCTIONS IN THIS FILE ARE INTERNAL FUNCTIONS WITH MUTABLE
   INTERFACES.  IT IS ONLY SAFE TO REACH THEM THROUGH DOCUMENTED INTERFACES.
   IN FACT, IT IS ALMOST GUARANTEED THAT THEY'LL CHANGE OR DISAPPEAR IN A
   FUTURE GNU MP RELEASE.


Copyright (C) 2000 Free Software Foundation, Inc.
Contributed by Paul Zimmermann.

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

/*
[1] Fast Recursive Division, by Christoph Burnikel and Joachim Ziegler,
    Technical report MPI-I-98-1-022, october 1998.
    http://www.mpi-sb.mpg.de/~ziegler/TechRep.ps.gz
*/

static mp_limb_t mpn_bz_div_3_halves_by_2
  _PROTO ((mp_ptr qp, mp_ptr np, mp_srcptr dp, mp_size_t n));


/* mpn_bz_divrem_n(n) calls 2*mul(n/2)+2*div(n/2), thus to be faster than
   div(n) = 4*div(n/2), we need mul(n/2) to be faster than the classic way,
   i.e. n/2 >= KARATSUBA_MUL_THRESHOLD */
#ifndef BZ_THRESHOLD
#define BZ_THRESHOLD (7 * KARATSUBA_MUL_THRESHOLD)
#endif

#if 0
static
unused_mpn_divrem (qp, qxn, np, nn, dp, dn)
     mp_ptr qp;
     mp_size_t qxn;
     mp_ptr np;
     mp_size_t nn;
     mp_srcptr dp;
     mp_size_t dn;
{
  /* This might be useful: */
  if (qxn != 0)
    {
      mp_limb_t c;
      mp_ptr tp = alloca ((nn + qxn) * BYTES_PER_MP_LIMB);
      MPN_COPY (tp + qxn - nn, np, nn);
      MPN_ZERO (tp, qxn);
      c = mpn_divrem (qp, 0L, tp, nn + qxn, dp, dn);
      /* Maybe copy proper part of tp to np?  Documentation is unclear about
	 the returned np value when qxn != 0 */
      return c;
    }
}
#endif


/* mpn_bz_divrem_n - Implements algorithm of page 8 in [1]: divides (np,2n)
   by (dp,n) and puts the quotient in (qp,n), the remainder in (np,n).
   Returns most significant limb of the quotient, which is 0 or 1.
   Requires that the most significant bit of the divisor is set.  */

mp_limb_t
#if __STDC__
mpn_bz_divrem_n (mp_ptr qp, mp_ptr np, mp_srcptr dp, mp_size_t n)
#else
mpn_bz_divrem_n (qp, np, dp, n)
     mp_ptr qp;
     mp_ptr np;
     mp_srcptr dp;
     mp_size_t n;
#endif
{
  mp_limb_t qhl, cc;

  if (n % 2 != 0)
    {
      qhl = mpn_bz_divrem_n (qp + 1, np + 2, dp + 1, n - 1);
      cc = mpn_submul_1 (np + 1, qp + 1, n - 1, dp[0]);
      cc = mpn_sub_1 (np + n, np + n, 1, cc);
      if (qhl) cc += mpn_sub_1 (np + n, np + n, 1, dp[0]);
      while (cc)
        {
          qhl -= mpn_sub_1 (qp + 1, qp + 1, n - 1, (mp_limb_t) 1);
          cc -= mpn_add_n (np + 1, np + 1, dp, n);
        }
      qhl += mpn_add_1 (qp + 1, qp + 1, n - 1,
                        mpn_sb_divrem_mn (qp, np, n + 1, dp, n));
    }
  else
    {
      mp_size_t n2 = n/2;
      qhl = mpn_bz_div_3_halves_by_2 (qp + n2, np + n2, dp, n2);
      qhl += mpn_add_1 (qp + n2, qp + n2, n2,
                        mpn_bz_div_3_halves_by_2 (qp, np, dp, n2));
    }
  return qhl;
}


/* divides (np, 3n) by (dp, 2n) and puts the quotient in (qp, n),
   the remainder in (np, 2n) */

static mp_limb_t
#if __STDC__
mpn_bz_div_3_halves_by_2 (mp_ptr qp, mp_ptr np, mp_srcptr dp, mp_size_t n)
#else
mpn_bz_div_3_halves_by_2 (qp, np, dp, n)
     mp_ptr qp;
     mp_ptr np;
     mp_srcptr dp;
     mp_size_t n;
#endif
{
  mp_size_t twon = n + n; 
  mp_limb_t qhl, cc;
  mp_ptr tmp;
  TMP_DECL (marker);

  TMP_MARK (marker);
  if (n < BZ_THRESHOLD)
    qhl = mpn_sb_divrem_mn (qp, np + n, twon, dp + n, n);
  else
    qhl = mpn_bz_divrem_n (qp, np + n, dp + n, n);
  tmp = (mp_ptr) TMP_ALLOC (twon * BYTES_PER_MP_LIMB);
  mpn_mul_n (tmp, qp, dp, n);
  cc = mpn_sub_n (np, np, tmp, twon);
  TMP_FREE (marker);
  if (qhl) cc += mpn_sub_n (np + n, np + n, dp, n);
  while (cc)
    {
      qhl -= mpn_sub_1 (qp, qp, n, (mp_limb_t) 1);
      cc -= mpn_add_n (np, np, dp, twon);
    }
  return qhl;
}
