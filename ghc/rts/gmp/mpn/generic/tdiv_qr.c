/* mpn_tdiv_qr -- Divide the numerator (np,nn) by the denominator (dp,dn) and
   write the nn-dn+1 quotient limbs at qp and the dn remainder limbs at rp.  If
   qxn is non-zero, generate that many fraction limbs and append them after the
   other quotient limbs, and update the remainder accordningly.  The input
   operands are unaffected.

   Preconditions:
   1. The most significant limb of of the divisor must be non-zero.
   2. No argument overlap is permitted.  (??? relax this ???)
   3. nn >= dn, even if qxn is non-zero.  (??? relax this ???)

   The time complexity of this is O(qn*qn+M(dn,qn)), where M(m,n) is the time
   complexity of multiplication.

Copyright (C) 1997, 2000 Free Software Foundation, Inc.

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

#ifndef BZ_THRESHOLD
#define BZ_THRESHOLD (7 * KARATSUBA_MUL_THRESHOLD)
#endif

/* Extract the middle limb from ((h,,l) << cnt) */
#define SHL(h,l,cnt) \
  ((h << cnt) | ((l >> 1) >> ((~cnt) & (BITS_PER_MP_LIMB - 1))))

void
#if __STDC__
mpn_tdiv_qr (mp_ptr qp, mp_ptr rp, mp_size_t qxn,
	     mp_srcptr np, mp_size_t nn, mp_srcptr dp, mp_size_t dn)
#else
mpn_tdiv_qr (qp, rp, qxn, np, nn, dp, dn)
     mp_ptr qp;
     mp_ptr rp;
     mp_size_t qxn;
     mp_srcptr np;
     mp_size_t nn;
     mp_srcptr dp;
     mp_size_t dn;
#endif
{
  /* FIXME:
     1. qxn
     2. pass allocated storage in additional parameter?
  */
  if (qxn != 0)
    abort ();

  switch (dn)
    {
    case 0:
      DIVIDE_BY_ZERO;

    case 1:
      {
	rp[0] = mpn_divmod_1 (qp, np, nn, dp[0]);
	return;
      }

    case 2:
      {
	int cnt;
	mp_ptr n2p, d2p;
	mp_limb_t qhl, cy;
	TMP_DECL (marker);
	TMP_MARK (marker);
	count_leading_zeros (cnt, dp[dn - 1]);
	if (cnt != 0)
	  {
	    d2p = (mp_ptr) TMP_ALLOC (dn * BYTES_PER_MP_LIMB);
	    mpn_lshift (d2p, dp, dn, cnt);
	    n2p = (mp_ptr) TMP_ALLOC ((nn + 1) * BYTES_PER_MP_LIMB);
	    cy = mpn_lshift (n2p, np, nn, cnt);
	    n2p[nn] = cy;
	    qhl = mpn_divrem_2 (qp, 0L, n2p, nn + (cy != 0), d2p);
	    if (cy == 0)
	      qp[nn - 2] = qhl;	/* always store nn-dn+1 quotient limbs */
	  }
	else
	  {
	    d2p = (mp_ptr) dp;
	    n2p = (mp_ptr) TMP_ALLOC (nn * BYTES_PER_MP_LIMB);
	    MPN_COPY (n2p, np, nn);
	    qhl = mpn_divrem_2 (qp, 0L, n2p, nn, d2p);
	    qp[nn - 2] = qhl;	/* always store nn-dn+1 quotient limbs */
	  }

	if (cnt != 0)
	  mpn_rshift (rp, n2p, dn, cnt);
	else
	  MPN_COPY (rp, n2p, dn);
	TMP_FREE (marker);
	return;
      }

    default:
      {
	int adjust;
	TMP_DECL (marker);
	TMP_MARK (marker);
	adjust = np[nn - 1] >= dp[dn - 1];	/* conservative tests for quotient size */
	if (nn + adjust >= 2 * dn)
	  {
	    mp_ptr n2p, d2p;
	    mp_limb_t cy;
	    int cnt;
	    count_leading_zeros (cnt, dp[dn - 1]);

	    qp[nn - dn] = 0;			/* zero high quotient limb */
	    if (cnt != 0)			/* normalize divisor if needed */
	      {
		d2p = (mp_ptr) TMP_ALLOC (dn * BYTES_PER_MP_LIMB);
		mpn_lshift (d2p, dp, dn, cnt);
		n2p = (mp_ptr) TMP_ALLOC ((nn + 1) * BYTES_PER_MP_LIMB);
		cy = mpn_lshift (n2p, np, nn, cnt);
		n2p[nn] = cy;
		nn += adjust;
	      }
	    else
	      {
		d2p = (mp_ptr) dp;
		n2p = (mp_ptr) TMP_ALLOC ((nn + 1) * BYTES_PER_MP_LIMB);
		MPN_COPY (n2p, np, nn);
		n2p[nn] = 0;
		nn += adjust;
	      }

	    if (dn == 2)
	      mpn_divrem_2 (qp, 0L, n2p, nn, d2p);
	    else if (dn < BZ_THRESHOLD)
	      mpn_sb_divrem_mn (qp, n2p, nn, d2p, dn);
	    else
	      {
		/* Perform 2*dn / dn limb divisions as long as the limbs
		   in np last.  */
		mp_ptr q2p = qp + nn - 2 * dn;
		n2p += nn - 2 * dn;
		mpn_bz_divrem_n (q2p, n2p, d2p, dn);
		nn -= dn;
		while (nn >= 2 * dn)
		  {
		    mp_limb_t c;
		    q2p -= dn;  n2p -= dn;
		    c = mpn_bz_divrem_n (q2p, n2p, d2p, dn);
		    ASSERT_ALWAYS (c == 0);
		    nn -= dn;
		  }

		if (nn != dn)
		  {
		    n2p -= nn - dn;
		    /* In theory, we could fall out to the cute code below
		       since we now have exactly the situation that code
		       is designed to handle.  We botch this badly and call
		       the basic mpn_sb_divrem_mn!  */
		    if (dn == 2)
		      mpn_divrem_2 (qp, 0L, n2p, nn, d2p);
		    else
		      mpn_sb_divrem_mn (qp, n2p, nn, d2p, dn);
		  }
	      }


	    if (cnt != 0)
	      mpn_rshift (rp, n2p, dn, cnt);
	    else
	      MPN_COPY (rp, n2p, dn);
	    TMP_FREE (marker);
	    return;
	  }

	/* When we come here, the numerator/partial remainder is less
	   than twice the size of the denominator.  */

	  {
	    /* Problem:

	       Divide a numerator N with nn limbs by a denominator D with dn
	       limbs forming a quotient of nn-dn+1 limbs.  When qn is small
	       compared to dn, conventional division algorithms perform poorly.
	       We want an algorithm that has an expected running time that is
	       dependent only on qn.  It is assumed that the most significant
	       limb of the numerator is smaller than the most significant limb
	       of the denominator.

	       Algorithm (very informally stated):

	       1) Divide the 2 x qn most significant limbs from the numerator
		  by the qn most significant limbs from the denominator.  Call
		  the result qest.  This is either the correct quotient, but
		  might be 1 or 2 too large.  Compute the remainder from the
		  division.  (This step is implemented by a mpn_divrem call.)

	       2) Is the most significant limb from the remainder < p, where p
		  is the product of the most significant limb from the quotient
		  and the next(d).  (Next(d) denotes the next ignored limb from
		  the denominator.)  If it is, decrement qest, and adjust the
		  remainder accordingly.

	       3) Is the remainder >= qest?  If it is, qest is the desired
		  quotient.  The algorithm terminates.

	       4) Subtract qest x next(d) from the remainder.  If there is
		  borrow out, decrement qest, and adjust the remainder
		  accordingly.

	       5) Skip one word from the denominator (i.e., let next(d) denote
		  the next less significant limb.  */

	    mp_size_t qn;
	    mp_ptr n2p, d2p;
	    mp_ptr tp;
	    mp_limb_t cy;
	    mp_size_t in, rn;
	    mp_limb_t quotient_too_large;
	    int cnt;

	    qn = nn - dn;
	    qp[qn] = 0;				/* zero high quotient limb */
	    qn += adjust;			/* qn cannot become bigger */

	    if (qn == 0)
	      {
		MPN_COPY (rp, np, dn);
		TMP_FREE (marker);
		return;
	      }

	    in = dn - qn;		/* (at least partially) ignored # of limbs in ops */
	    /* Normalize denominator by shifting it to the left such that its
	       most significant bit is set.  Then shift the numerator the same
	       amount, to mathematically preserve quotient.  */
	    count_leading_zeros (cnt, dp[dn - 1]);
	    if (cnt != 0)
	      {
		d2p = (mp_ptr) TMP_ALLOC (qn * BYTES_PER_MP_LIMB);

		mpn_lshift (d2p, dp + in, qn, cnt);
		d2p[0] |= dp[in - 1] >> (BITS_PER_MP_LIMB - cnt);

		n2p = (mp_ptr) TMP_ALLOC ((2 * qn + 1) * BYTES_PER_MP_LIMB);
		cy = mpn_lshift (n2p, np + nn - 2 * qn, 2 * qn, cnt);
		if (adjust)
		  {
		    n2p[2 * qn] = cy;
		    n2p++;
		  }
		else
		  {
		    n2p[0] |= np[nn - 2 * qn - 1] >> (BITS_PER_MP_LIMB - cnt);
		  }
	      }
	    else
	      {
		d2p = (mp_ptr) dp + in;

		n2p = (mp_ptr) TMP_ALLOC ((2 * qn + 1) * BYTES_PER_MP_LIMB);
		MPN_COPY (n2p, np + nn - 2 * qn, 2 * qn);
		if (adjust)
		  {
		    n2p[2 * qn] = 0;
		    n2p++;
		  }
	      }

	    /* Get an approximate quotient using the extracted operands.  */
	    if (qn == 1)
	      {
		mp_limb_t q0, r0;
		mp_limb_t gcc272bug_n1, gcc272bug_n0, gcc272bug_d0;
		/* Due to a gcc 2.7.2.3 reload pass bug, we have to use some
		   temps here.  This doesn't hurt code quality on any machines
		   so we do it unconditionally.  */
		gcc272bug_n1 = n2p[1];
		gcc272bug_n0 = n2p[0];
		gcc272bug_d0 = d2p[0];
		udiv_qrnnd (q0, r0, gcc272bug_n1, gcc272bug_n0, gcc272bug_d0);
		n2p[0] = r0;
		qp[0] = q0;
	      }
	    else if (qn == 2)
	      mpn_divrem_2 (qp, 0L, n2p, 4L, d2p);
	    else if (qn < BZ_THRESHOLD)
	      mpn_sb_divrem_mn (qp, n2p, qn * 2, d2p, qn);
	    else
	      mpn_bz_divrem_n (qp, n2p, d2p, qn);

	    rn = qn;
	    /* Multiply the first ignored divisor limb by the most significant
	       quotient limb.  If that product is > the partial remainder's
	       most significant limb, we know the quotient is too large.  This
	       test quickly catches most cases where the quotient is too large;
	       it catches all cases where the quotient is 2 too large.  */
	    {
	      mp_limb_t dl, x;
	      mp_limb_t h, l;

	      if (in - 2 < 0)
		dl = 0;
	      else
		dl = dp[in - 2];

	      x = SHL (dp[in - 1], dl, cnt);
	      umul_ppmm (h, l, x, qp[qn - 1]);

	      if (n2p[qn - 1] < h)
		{
		  mp_limb_t cy;

		  mpn_decr_u (qp, (mp_limb_t) 1);
		  cy = mpn_add_n (n2p, n2p, d2p, qn);
		  if (cy)
		    {
		      /* The partial remainder is safely large.  */
		      n2p[qn] = cy;
		      ++rn;
		    }
		}
	    }

	    quotient_too_large = 0;
	    if (cnt != 0)
	      {
		mp_limb_t cy1, cy2;

		/* Append partially used numerator limb to partial remainder.  */
		cy1 = mpn_lshift (n2p, n2p, rn, BITS_PER_MP_LIMB - cnt);
		n2p[0] |= np[in - 1] & (~(mp_limb_t) 0 >> cnt);

		/* Update partial remainder with partially used divisor limb.  */
		cy2 = mpn_submul_1 (n2p, qp, qn, dp[in - 1] & (~(mp_limb_t) 0 >> cnt));
		if (qn != rn)
		  {
		    if (n2p[qn] < cy2)
		      abort ();
		    n2p[qn] -= cy2;
		  }
		else
		  {
		    n2p[qn] = cy1 - cy2;

		    quotient_too_large = (cy1 < cy2);
		    ++rn;
		  }
		--in;
	      }
	    /* True: partial remainder now is neutral, i.e., it is not shifted up.  */

	    tp = (mp_ptr) TMP_ALLOC (dn * BYTES_PER_MP_LIMB);

	    if (in < qn)
	      {
		if (in == 0)
		  {
		    MPN_COPY (rp, n2p, rn);
		    if (rn != dn)
		      abort ();
		    goto foo;
		  }
		mpn_mul (tp, qp, qn, dp, in);
	      }
	    else
	      mpn_mul (tp, dp, in, qp, qn);

	    cy = mpn_sub (n2p, n2p, rn, tp + in, qn);
	    MPN_COPY (rp + in, n2p, dn - in);
	    quotient_too_large |= cy;
	    cy = mpn_sub_n (rp, np, tp, in);
	    cy = mpn_sub_1 (rp + in, rp + in, rn, cy);
	    quotient_too_large |= cy;
	  foo:
	    if (quotient_too_large)
	      {
		mpn_decr_u (qp, (mp_limb_t) 1);
		mpn_add_n (rp, rp, dp, dn);
	      }
	  }
	TMP_FREE (marker);
	return;
      }
    }
}
