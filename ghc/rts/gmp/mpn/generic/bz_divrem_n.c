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

static mp_limb_t mpn_bz_div_3_halves_by_2 _PROTO ((mp_ptr, mp_ptr, mp_srcptr,
                                                   mp_size_t, mp_ptr));

static mp_limb_t mpn_bz_divrem_aux _PROTO ((mp_ptr, mp_ptr, mp_srcptr,
                                            mp_size_t, mp_ptr));

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
  mp_limb_t qhl = 0;
  if (mpn_cmp (np + n, dp, n) >= 0)
    {
      qhl = 1;
      mpn_sub_n (np + n, np + n, dp, n);
      abort ();
    }
  if (n % 2 != 0)
    {
      /* divide (2n - 2) most significant limbs from np by those (n - 1) from dp */
      if (n < BZ_THRESHOLD)
	qhl += mpn_sb_divrem_mn (qp + 1, np + 2, 2 * (n - 1), dp + 1, n - 1);
      else
	qhl += mpn_bz_divrem_n (qp + 1, np + 2, dp + 1, n - 1);
      /* now (qp + 1, n - 1) contains the quotient of (np + 2, 2n - 2) by
	 (dp + 1, n - 1) and (np + 2, n - 1) contains the remainder */
      if (mpn_sub_1 (np + n, np + n, 1,
		     mpn_submul_1 (np + 1, qp + 1, n - 1, dp[0])))
	{
	  /* quotient too large */
	  qhl -= mpn_sub_1 (qp + 1, qp + 1, n - 1, 1L);
	  if (mpn_add_n (np + 1, np + 1, dp, n) == 0)
	    { /* still too large */
	      qhl -= mpn_sub_1 (qp + 1, qp + 1, n - 1, 1L);
	      mpn_add_n (np + 1, np + 1, dp, n); /* always carry here */
	    }
	}
      /* now divide (np, n + 1) by (dp, n) */
      qhl += mpn_add_1 (qp + 1, qp + 1, n - 1,
			mpn_sb_divrem_mn (qp, np, n + 1, dp, n));
    }
  else
    {
      mp_ptr tmp;
      mp_size_t n2 = n/2;
      TMP_DECL (marker);
      TMP_MARK (marker);
      tmp = (mp_ptr) TMP_ALLOC (n * BYTES_PER_MP_LIMB);
      qhl = mpn_bz_div_3_halves_by_2 (qp + n2, np + n2, dp, n2, tmp);
      qhl += mpn_add_1 (qp + n2, qp + n2, n2,
		       mpn_bz_div_3_halves_by_2 (qp, np, dp, n2, tmp));
      TMP_FREE (marker);
    }
  return qhl;
}

/* Like mpn_bz_divrem_n, but without memory allocation.  Also
   assumes mpn_cmp (np + n, dp, n) < 0 */

static mp_limb_t
#if __STDC__
mpn_bz_divrem_aux (mp_ptr qp, mp_ptr np, mp_srcptr dp, mp_size_t n, mp_ptr tmp)
#else
mpn_bz_divrem_aux (qp, np, dp, n, tmp)
     mp_ptr qp;
     mp_ptr np;
     mp_srcptr dp;
     mp_size_t n;
     mp_ptr tmp;
#endif
{
  mp_limb_t qhl;

  if (n % 2 != 0)
    {
      /* divide (2n - 2) most significant limbs from np by those (n - 1) from dp */
      qhl = mpn_bz_divrem_aux (qp + 1, np + 2, dp + 1, n - 1, tmp);
      /* now (qp + 1, n - 1) contains the quotient of (np + 2, 2n - 2) by
	 (dp + 1, n - 1) and (np + 2, n - 1) contains the remainder */
      if (mpn_sub_1 (np + n, np + n, 1,
		     mpn_submul_1 (np + 1, qp + 1, n - 1, dp[0])))
	{
	  /* quotient too large */
	  qhl -= mpn_sub_1 (qp + 1, qp + 1, n - 1, 1L);
	  if (mpn_add_n (np + 1, np + 1, dp, n) == 0)
	    { /* still too large */
	      qhl -= mpn_sub_1 (qp + 1, qp + 1, n - 1, 1L);
	      mpn_add_n (np + 1, np + 1, dp, n); /* always carry here */
	    }
	}
      /* now divide (np, n + 1) by (dp, n) */
      qhl += mpn_add_1 (qp + 1, qp + 1, n - 1,
			mpn_sb_divrem_mn (qp, np, n + 1, dp, n));
    }
  else
    {
      mp_size_t n2 = n/2;
      qhl = mpn_bz_div_3_halves_by_2 (qp + n2, np + n2, dp, n2, tmp);
      qhl += mpn_add_1 (qp + n2, qp + n2, n2,
		       mpn_bz_div_3_halves_by_2 (qp, np, dp, n2, tmp));
    }
  return qhl;
}

/* divides (np, 3n) by (dp, 2n) and puts the quotient in (qp, n),
   the remainder in (np, 2n) */

static mp_limb_t
#if __STDC__
mpn_bz_div_3_halves_by_2 (mp_ptr qp, mp_ptr np, mp_srcptr dp, mp_size_t n,
                          mp_ptr tmp)
#else
mpn_bz_div_3_halves_by_2 (qp, np, dp, n, tmp)
     mp_ptr qp;
     mp_ptr np;
     mp_srcptr dp;
     mp_size_t n;
     mp_ptr tmp;
#endif
{
  mp_size_t twon = n + n;
  mp_limb_t qhl;

  if (n < BZ_THRESHOLD)
    qhl = mpn_sb_divrem_mn (qp, np + n, twon, dp + n, n);
  else
    qhl = mpn_bz_divrem_aux (qp, np + n, dp + n, n, tmp);
  /* q = (qp, n), c = (np + n, n) with the notations of [1] */
  mpn_mul_n (tmp, qp, dp, n);
  if (qhl != 0)
    mpn_add_n (tmp + n, tmp + n, dp, n);
  if (mpn_sub_n (np, np, tmp, twon))		/* R = (np, 2n) */
    {
      qhl -= mpn_sub_1 (qp, qp, n, 1L);
      if (mpn_add_n (np, np, dp, twon) == 0)
	{ /* qp still too large */
	  qhl -= mpn_sub_1 (qp, qp, n, 1L);
	  mpn_add_n (np, np, dp, twon);		/* always carry here */
	}
    }
  return qhl;
}
