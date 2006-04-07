/* mpn_gcdext -- Extended Greatest Common Divisor.

Copyright (C) 1996, 1998, 2000 Free Software Foundation, Inc.

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

#ifndef GCDEXT_THRESHOLD
#define GCDEXT_THRESHOLD 17
#endif

#ifndef EXTEND
#define EXTEND 1
#endif

#if STAT
int arr[BITS_PER_MP_LIMB];
#endif


/* mpn_gcdext (GP, SP, SSIZE, UP, USIZE, VP, VSIZE)

   Compute the extended GCD of {UP,USIZE} and {VP,VSIZE} and store the
   greatest common divisor at GP (unless it is 0), and the first cofactor at
   SP.  Write the size of the cofactor through the pointer SSIZE.  Return the
   size of the value at GP.  Note that SP might be a negative number; this is
   denoted by storing the negative of the size through SSIZE.

   {UP,USIZE} and {VP,VSIZE} are both clobbered.

   The space allocation for all four areas needs to be USIZE+1.

   Preconditions: 1) U >= V.
		  2) V > 0.  */

/* We use Lehmer's algorithm.  The idea is to extract the most significant
   bits of the operands, and compute the continued fraction for them.  We then
   apply the gathered cofactors to the full operands.

   Idea 1: After we have performed a full division, don't shift operands back,
	   but instead account for the extra factors-of-2 thus introduced.
   Idea 2: Simple generalization to use divide-and-conquer would give us an
	   algorithm that runs faster than O(n^2).
   Idea 3: The input numbers need less space as the computation progresses,
	   while the s0 and s1 variables need more space.  To save memory, we
	   could make them share space, and have the latter variables grow
	   into the former.
   Idea 4: We should not do double-limb arithmetic from the start.  Instead,
	   do things in single-limb arithmetic until the quotients differ,
	   and then switch to double-limb arithmetic.  */


/* Division optimized for small quotients.  If the quotient is more than one limb,
   store 1 in *qh and return 0.  */
static mp_limb_t
#if __STDC__
div2 (mp_limb_t *qh, mp_limb_t n1, mp_limb_t n0, mp_limb_t d1, mp_limb_t d0)
#else
div2 (qh, n1, n0, d1, d0)
     mp_limb_t *qh;
     mp_limb_t n1;
     mp_limb_t n0;
     mp_limb_t d1;
     mp_limb_t d0;
#endif
{
  if (d1 == 0)
    {
      *qh = 1;
      return 0;
    }

  if ((mp_limb_signed_t) n1 < 0)
    {
      mp_limb_t q;
      int cnt;
      for (cnt = 1; (mp_limb_signed_t) d1 >= 0; cnt++)
	{
	  d1 = (d1 << 1) | (d0 >> (BITS_PER_MP_LIMB - 1));
	  d0 = d0 << 1;
	}

      q = 0;
      while (cnt)
	{
	  q <<= 1;
	  if (n1 > d1 || (n1 == d1 && n0 >= d0))
	    {
	      sub_ddmmss (n1, n0, n1, n0, d1, d0);
	      q |= 1;
	    }
	  d0 = (d1 << (BITS_PER_MP_LIMB - 1)) | (d0 >> 1);
	  d1 = d1 >> 1;
	  cnt--;
	}

      *qh = 0;
      return q;
    }
  else
    {
      mp_limb_t q;
      int cnt;
      for (cnt = 0; n1 > d1 || (n1 == d1 && n0 >= d0); cnt++)
	{
	  d1 = (d1 << 1) | (d0 >> (BITS_PER_MP_LIMB - 1));
	  d0 = d0 << 1;
	}

      q = 0;
      while (cnt)
	{
	  d0 = (d1 << (BITS_PER_MP_LIMB - 1)) | (d0 >> 1);
	  d1 = d1 >> 1;
	  q <<= 1;
	  if (n1 > d1 || (n1 == d1 && n0 >= d0))
	    {
	      sub_ddmmss (n1, n0, n1, n0, d1, d0);
	      q |= 1;
	    }
	  cnt--;
	}

      *qh = 0;
      return q;
    }
}

mp_size_t
#if EXTEND
#if __STDC__
mpn_gcdext (mp_ptr gp, mp_ptr s0p, mp_size_t *s0size,
	    mp_ptr up, mp_size_t size, mp_ptr vp, mp_size_t vsize)
#else
mpn_gcdext (gp, s0p, s0size, up, size, vp, vsize)
     mp_ptr gp;
     mp_ptr s0p;
     mp_size_t *s0size;
     mp_ptr up;
     mp_size_t size;
     mp_ptr vp;
     mp_size_t vsize;
#endif
#else
#if __STDC__
mpn_gcd (mp_ptr gp,
	 mp_ptr up, mp_size_t size, mp_ptr vp, mp_size_t vsize)
#else
mpn_gcd (gp, up, size, vp, vsize)
     mp_ptr gp;
     mp_ptr up;
     mp_size_t size;
     mp_ptr vp;
     mp_size_t vsize;
#endif
#endif
{
  mp_limb_t A, B, C, D;
  int cnt;
  mp_ptr tp, wp;
#if RECORD
  mp_limb_t max = 0;
#endif
#if EXTEND
  mp_ptr s1p;
  mp_ptr orig_s0p = s0p;
  mp_size_t ssize;
  int sign = 1;
#endif
  int use_double_flag;
  TMP_DECL (mark);

  TMP_MARK (mark);

  use_double_flag = (size >= GCDEXT_THRESHOLD);

  tp = (mp_ptr) TMP_ALLOC ((size + 1) * BYTES_PER_MP_LIMB);
  wp = (mp_ptr) TMP_ALLOC ((size + 1) * BYTES_PER_MP_LIMB);
#if EXTEND
  s1p = (mp_ptr) TMP_ALLOC ((size + 1) * BYTES_PER_MP_LIMB);

  MPN_ZERO (s0p, size);
  MPN_ZERO (s1p, size);

  s0p[0] = 1;
  s1p[0] = 0;
  ssize = 1;
#endif

  if (size > vsize)
    {
      /* Normalize V (and shift up U the same amount).  */
      count_leading_zeros (cnt, vp[vsize - 1]);
      if (cnt != 0)
	{
	  mp_limb_t cy;
	  mpn_lshift (vp, vp, vsize, cnt);
	  cy = mpn_lshift (up, up, size, cnt);
	  up[size] = cy;
	  size += cy != 0;
	}

      mpn_divmod (up + vsize, up, size, vp, vsize);
#if EXTEND
      /* This is really what it boils down to in this case... */
      s0p[0] = 0;
      s1p[0] = 1;
      sign = -sign;
#endif
      size = vsize;
      if (cnt != 0)
	{
	  mpn_rshift (up, up, size, cnt);
	  mpn_rshift (vp, vp, size, cnt);
	}
      MP_PTR_SWAP (up, vp);
    }

  for (;;)
    {
      mp_limb_t asign;
      /* Figure out exact size of V.  */
      vsize = size;
      MPN_NORMALIZE (vp, vsize);
      if (vsize <= 1)
	break;

      if (use_double_flag)
	{
	  mp_limb_t uh, vh, ul, vl;
	  /* Let UH,UL be the most significant limbs of U, and let VH,VL be
	     the corresponding bits from V.  */
	  uh = up[size - 1];
	  vh = vp[size - 1];
	  ul = up[size - 2];
	  vl = vp[size - 2];
	  count_leading_zeros (cnt, uh);
	  if (cnt != 0)
	    {
	      uh = (uh << cnt) | (ul >> (BITS_PER_MP_LIMB - cnt));
	      vh = (vh << cnt) | (vl >> (BITS_PER_MP_LIMB - cnt));
	      vl <<= cnt;
	      ul <<= cnt;
	      if (size >= 3)
		{
		  ul |= (up[size - 3] >> (BITS_PER_MP_LIMB - cnt));
		  vl |= (vp[size - 3] >> (BITS_PER_MP_LIMB - cnt));
		}
	    }

	  A = 1;
	  B = 0;
	  C = 0;
	  D = 1;

	  asign = 0;
	  for (;;)
	    {
	      mp_limb_t T;
	      mp_limb_t qh, q1, q2;
	      mp_limb_t nh, nl, dh, dl;
	      mp_limb_t t1, t0;
	      mp_limb_t Th, Tl;

	      sub_ddmmss (dh, dl, vh, vl, 0, C);
	      if ((dl | dh) == 0)
		break;
	      add_ssaaaa (nh, nl, uh, ul, 0, A);
	      q1 = div2 (&qh, nh, nl, dh, dl);
	      if (qh != 0)
		break;		/* could handle this */

	      add_ssaaaa (dh, dl, vh, vl, 0, D);
	      if ((dl | dh) == 0)
		break;
	      sub_ddmmss (nh, nl, uh, ul, 0, B);
	      q2 = div2 (&qh, nh, nl, dh, dl);
	      if (qh != 0)
		break;		/* could handle this */

	      if (q1 != q2)
		break;

	      asign = ~asign;

	      T = A + q1 * C;
	      A = C;
	      C = T;
	      T = B + q1 * D;
	      B = D;
	      D = T;
	      umul_ppmm (t1, t0, q1, vl);
	      t1 += q1 * vh;
	      sub_ddmmss (Th, Tl, uh, ul, t1, t0);
	      uh = vh, ul = vl;
	      vh = Th, vl = Tl;

	      add_ssaaaa (dh, dl, vh, vl, 0, C);
	      sub_ddmmss (nh, nl, uh, ul, 0, A);
	      q1 = div2 (&qh, nh, nl, dh, dl);
	      if (qh != 0)
		break;		/* could handle this */

	      sub_ddmmss (dh, dl, vh, vl, 0, D);
	      if ((dl | dh) == 0)
		break;
	      add_ssaaaa (nh, nl, uh, ul, 0, B);
	      q2 = div2 (&qh, nh, nl, dh, dl);
	      if (qh != 0)
		break;		/* could handle this */

	      if (q1 != q2)
		break;

	      asign = ~asign;

	      T = A + q1 * C;
	      A = C;
	      C = T;
	      T = B + q1 * D;
	      B = D;
	      D = T;
	      umul_ppmm (t1, t0, q1, vl);
	      t1 += q1 * vh;
	      sub_ddmmss (Th, Tl, uh, ul, t1, t0);
	      uh = vh, ul = vl;
	      vh = Th, vl = Tl;
	    }
#if EXTEND
	  if (asign)
	    sign = -sign;
#endif
	}
      else /* Same, but using single-limb calculations.  */
	{
	  mp_limb_t uh, vh;
	  /* Make UH be the most significant limb of U, and make VH be
	     corresponding bits from V.  */
	  uh = up[size - 1];
	  vh = vp[size - 1];
	  count_leading_zeros (cnt, uh);
	  if (cnt != 0)
	    {
	      uh = (uh << cnt) | (up[size - 2] >> (BITS_PER_MP_LIMB - cnt));
	      vh = (vh << cnt) | (vp[size - 2] >> (BITS_PER_MP_LIMB - cnt));
	    }

	  A = 1;
	  B = 0;
	  C = 0;
	  D = 1;

	  asign = 0;
	  for (;;)
	    {
	      mp_limb_t q, T;
	      if (vh - C == 0 || vh + D == 0)
		break;

	      q = (uh + A) / (vh - C);
	      if (q != (uh - B) / (vh + D))
		break;

	      asign = ~asign;

	      T = A + q * C;
	      A = C;
	      C = T;
	      T = B + q * D;
	      B = D;
	      D = T;
	      T = uh - q * vh;
	      uh = vh;
	      vh = T;

	      if (vh - D == 0)
		break;

	      q = (uh - A) / (vh + C);
	      if (q != (uh + B) / (vh - D))
		break;

	      asign = ~asign;

	      T = A + q * C;
	      A = C;
	      C = T;
	      T = B + q * D;
	      B = D;
	      D = T;
	      T = uh - q * vh;
	      uh = vh;
	      vh = T;
	    }
#if EXTEND
	  if (asign)
	    sign = -sign;
#endif
	}

#if RECORD
      max = MAX (A, max);  max = MAX (B, max);
      max = MAX (C, max);  max = MAX (D, max);
#endif

      if (B == 0)
	{
	  mp_limb_t qh;
	  mp_size_t i;
	  /* This is quite rare.  I.e., optimize something else!  */

	  /* Normalize V (and shift up U the same amount).  */
	  count_leading_zeros (cnt, vp[vsize - 1]);
	  if (cnt != 0)
	    {
	      mp_limb_t cy;
	      mpn_lshift (vp, vp, vsize, cnt);
	      cy = mpn_lshift (up, up, size, cnt);
	      up[size] = cy;
	      size += cy != 0;
	    }

	  qh = mpn_divmod (up + vsize, up, size, vp, vsize);
#if EXTEND
	  MPN_COPY (tp, s0p, ssize);
	  {
	    mp_size_t qsize;

	    qsize = size - vsize; /* size of stored quotient from division */
	    if (ssize < qsize)
	      {
		MPN_ZERO (tp + ssize, qsize - ssize);
		MPN_ZERO (s1p + ssize, qsize); /* zero s1 too */
		for (i = 0; i < ssize; i++)
		  {
		    mp_limb_t cy;
		    cy = mpn_addmul_1 (tp + i, up + vsize, qsize, s1p[i]);
		    tp[qsize + i] = cy;
		  }
		if (qh != 0)
		  {
		    mp_limb_t cy;
		    cy = mpn_add_n (tp + qsize, tp + qsize, s1p, ssize);
		    if (cy != 0)
		      abort ();
		  }
	      }
	    else
	      {
		MPN_ZERO (s1p + ssize, qsize); /* zero s1 too */
		for (i = 0; i < qsize; i++)
		  {
		    mp_limb_t cy;
		    cy = mpn_addmul_1 (tp + i, s1p, ssize, up[vsize + i]);
		    tp[ssize + i] = cy;
		  }
		if (qh != 0)
		  {
		    mp_limb_t cy;
		    cy = mpn_add_n (tp + qsize, tp + qsize, s1p, ssize);
		    if (cy != 0)
		      {
			tp[qsize + ssize] = cy;
			s1p[qsize + ssize] = 0;
			ssize++;
		      }
		  }
	      }
	    ssize += qsize;
	    ssize -= tp[ssize - 1] == 0;
	  }

	  sign = -sign;
	  MP_PTR_SWAP (s0p, s1p);
	  MP_PTR_SWAP (s1p, tp);
#endif
	  size = vsize;
	  if (cnt != 0)
	    {
	      mpn_rshift (up, up, size, cnt);
	      mpn_rshift (vp, vp, size, cnt);
	    }
	  MP_PTR_SWAP (up, vp);
	}
      else
	{
#if EXTEND
	  mp_size_t tsize, wsize;
#endif
	  /* T = U*A + V*B
	     W = U*C + V*D
	     U = T
	     V = W	   */

#if STAT
	  { mp_limb_t x; x = A | B | C | D; count_leading_zeros (cnt, x);
	  arr[BITS_PER_MP_LIMB - cnt]++; }
#endif
	  if (A == 0)
	    {
	      /* B == 1 and C == 1 (D is arbitrary) */
	      mp_limb_t cy;
	      MPN_COPY (tp, vp, size);
	      MPN_COPY (wp, up, size);
	      mpn_submul_1 (wp, vp, size, D);
	      MP_PTR_SWAP (tp, up);
	      MP_PTR_SWAP (wp, vp);
#if EXTEND
	      MPN_COPY (tp, s1p, ssize);
	      tsize = ssize;
	      tp[ssize] = 0;	/* must zero since wp might spill below */
	      MPN_COPY (wp, s0p, ssize);
	      cy = mpn_addmul_1 (wp, s1p, ssize, D);
	      wp[ssize] = cy;
	      wsize = ssize + (cy != 0);
	      MP_PTR_SWAP (tp, s0p);
	      MP_PTR_SWAP (wp, s1p);
	      ssize = MAX (wsize, tsize);
#endif
	    }
	  else
	    {
	      if (asign)
		{
		  mp_limb_t cy;
		  mpn_mul_1 (tp, vp, size, B);
		  mpn_submul_1 (tp, up, size, A);
		  mpn_mul_1 (wp, up, size, C);
		  mpn_submul_1 (wp, vp, size, D);
		  MP_PTR_SWAP (tp, up);
		  MP_PTR_SWAP (wp, vp);
#if EXTEND
		  cy = mpn_mul_1 (tp, s1p, ssize, B);
		  cy += mpn_addmul_1 (tp, s0p, ssize, A);
		  tp[ssize] = cy;
		  tsize = ssize + (cy != 0);
		  cy = mpn_mul_1 (wp, s0p, ssize, C);
		  cy += mpn_addmul_1 (wp, s1p, ssize, D);
		  wp[ssize] = cy;
		  wsize = ssize + (cy != 0);
		  MP_PTR_SWAP (tp, s0p);
		  MP_PTR_SWAP (wp, s1p);
		  ssize = MAX (wsize, tsize);
#endif
		}
	      else
		{
		  mp_limb_t cy;
		  mpn_mul_1 (tp, up, size, A);
		  mpn_submul_1 (tp, vp, size, B);
		  mpn_mul_1 (wp, vp, size, D);
		  mpn_submul_1 (wp, up, size, C);
		  MP_PTR_SWAP (tp, up);
		  MP_PTR_SWAP (wp, vp);
#if EXTEND
		  cy = mpn_mul_1 (tp, s0p, ssize, A);
		  cy += mpn_addmul_1 (tp, s1p, ssize, B);
		  tp[ssize] = cy;
		  tsize = ssize + (cy != 0);
		  cy = mpn_mul_1 (wp, s1p, ssize, D);
		  cy += mpn_addmul_1 (wp, s0p, ssize, C);
		  wp[ssize] = cy;
		  wsize = ssize + (cy != 0);
		  MP_PTR_SWAP (tp, s0p);
		  MP_PTR_SWAP (wp, s1p);
		  ssize = MAX (wsize, tsize);
#endif
		}
	    }

	  size -= up[size - 1] == 0;
	}
    }

#if RECORD
  printf ("max: %lx\n", max);
#endif

#if STAT
 {int i; for (i = 0; i < BITS_PER_MP_LIMB; i++) printf ("%d:%d\n", i, arr[i]);}
#endif

  if (vsize == 0)
    {
      if (gp != up && gp != 0)
	MPN_COPY (gp, up, size);
#if EXTEND
      MPN_NORMALIZE (s0p, ssize);
      if (orig_s0p != s0p)
	MPN_COPY (orig_s0p, s0p, ssize);
      *s0size = sign >= 0 ? ssize : -ssize;
#endif
      TMP_FREE (mark);
      return size;
    }
  else
    {
      mp_limb_t vl, ul, t;
#if EXTEND
      mp_size_t qsize, i;
#endif
      vl = vp[0];
#if EXTEND
      t = mpn_divmod_1 (wp, up, size, vl);

      MPN_COPY (tp, s0p, ssize);

      qsize = size - (wp[size - 1] == 0); /* size of quotient from division */
      if (ssize < qsize)
	{
	  MPN_ZERO (tp + ssize, qsize - ssize);
	  MPN_ZERO (s1p + ssize, qsize); /* zero s1 too */
	  for (i = 0; i < ssize; i++)
	    {
	      mp_limb_t cy;
	      cy = mpn_addmul_1 (tp + i, wp, qsize, s1p[i]);
	      tp[qsize + i] = cy;
	    }
	}
      else
	{
	  MPN_ZERO (s1p + ssize, qsize); /* zero s1 too */
	  for (i = 0; i < qsize; i++)
	    {
	      mp_limb_t cy;
	      cy = mpn_addmul_1 (tp + i, s1p, ssize, wp[i]);
	      tp[ssize + i] = cy;
	    }
	}
      ssize += qsize;
      ssize -= tp[ssize - 1] == 0;

      sign = -sign;
      MP_PTR_SWAP (s0p, s1p);
      MP_PTR_SWAP (s1p, tp);
#else
      t = mpn_mod_1 (up, size, vl);
#endif
      ul = vl;
      vl = t;
      while (vl != 0)
	{
	  mp_limb_t t;
#if EXTEND
	  mp_limb_t q;
	  q = ul / vl;
	  t = ul - q * vl;

	  MPN_COPY (tp, s0p, ssize);

	  MPN_ZERO (s1p + ssize, 1); /* zero s1 too */

	  {
	    mp_limb_t cy;
	    cy = mpn_addmul_1 (tp, s1p, ssize, q);
	    tp[ssize] = cy;
	  }

	  ssize += 1;
	  ssize -= tp[ssize - 1] == 0;

	  sign = -sign;
	  MP_PTR_SWAP (s0p, s1p);
	  MP_PTR_SWAP (s1p, tp);
#else
	  t = ul % vl;
#endif
	  ul = vl;
	  vl = t;
	}
      if (gp != 0)
	gp[0] = ul;
#if EXTEND
      MPN_NORMALIZE (s0p, ssize);
      if (orig_s0p != s0p)
	MPN_COPY (orig_s0p, s0p, ssize);
      *s0size = sign >= 0 ? ssize : -ssize;
#endif
      TMP_FREE (mark);
      return 1;
    }
}
