/* mpz_gcdext(g, s, t, a, b) -- Set G to gcd(a, b), and S and T such that
   g = as + bt.

Copyright (C) 1991, 1993, 1994, 1995, 1996, 1997, 2000 Free Software
Foundation, Inc.

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

#include <stdio.h> /* for NULL */
#include "gmp.h"
#include "gmp-impl.h"

void
#if __STDC__
mpz_gcdext (mpz_ptr g, mpz_ptr s, mpz_ptr t, mpz_srcptr a, mpz_srcptr b)
#else
mpz_gcdext (g, s, t, a, b)
     mpz_ptr g;
     mpz_ptr s;
     mpz_ptr t;
     mpz_srcptr a;
     mpz_srcptr b;
#endif
{
  mp_size_t asize, bsize, usize, vsize;
  mp_srcptr ap, bp;
  mp_ptr up, vp;
  mp_size_t gsize, ssize, tmp_ssize;
  mp_ptr gp, sp, tmp_gp, tmp_sp;
  mpz_srcptr u, v;
  mpz_ptr ss, tt;
  __mpz_struct stmp, gtmp;
  TMP_DECL (marker);

  TMP_MARK (marker);

  /* mpn_gcdext requires that U >= V.  Therefore, we often have to swap U and
     V.  This in turn leads to a lot of complications.  The computed cofactor
     will be the wrong one, so we have to fix that up at the end.  */

  asize = ABS (SIZ (a));
  bsize = ABS (SIZ (b));
  ap = PTR (a);
  bp = PTR (b);
  if (asize > bsize || (asize == bsize && mpn_cmp (ap, bp, asize) > 0))
    {
      usize = asize;
      vsize = bsize;
      up = (mp_ptr) TMP_ALLOC ((usize + 1) * BYTES_PER_MP_LIMB);
      vp = (mp_ptr) TMP_ALLOC ((vsize + 1) * BYTES_PER_MP_LIMB);
      MPN_COPY (up, ap, usize);
      MPN_COPY (vp, bp, vsize);
      u = a;
      v = b;
      ss = s;
      tt = t;
    }
  else
    {
      usize = bsize;
      vsize = asize;
      up = (mp_ptr) TMP_ALLOC ((usize + 1) * BYTES_PER_MP_LIMB);
      vp = (mp_ptr) TMP_ALLOC ((vsize + 1) * BYTES_PER_MP_LIMB);
      MPN_COPY (up, bp, usize);
      MPN_COPY (vp, ap, vsize);
      u = b;
      v = a;
      ss = t;
      tt = s;
    }

  tmp_gp = (mp_ptr) TMP_ALLOC ((usize + 1) * BYTES_PER_MP_LIMB);
  tmp_sp = (mp_ptr) TMP_ALLOC ((usize + 1) * BYTES_PER_MP_LIMB);

  if (vsize == 0)
    {
      tmp_sp[0] = 1;
      tmp_ssize = 1;
      MPN_COPY (tmp_gp, up, usize);
      gsize = usize;
    }
  else
    gsize = mpn_gcdext (tmp_gp, tmp_sp, &tmp_ssize, up, usize, vp, vsize);
  ssize = ABS (tmp_ssize);

  PTR (&gtmp) = tmp_gp;
  SIZ (&gtmp) = gsize;

  PTR (&stmp) = tmp_sp;
  SIZ (&stmp) = (tmp_ssize ^ SIZ (u)) >= 0 ? ssize : -ssize;

  if (tt != NULL)
    {
      if (SIZ (v) == 0)
	SIZ (tt) = 0;
      else
	{
	  mpz_t x;
	  MPZ_TMP_INIT (x, ssize + usize + 1);
	  mpz_mul (x, &stmp, u);
	  mpz_sub (x, &gtmp, x);
	  mpz_tdiv_q (tt, x, v);
	}
    }

  if (ss != NULL)
    {
      if (ALLOC (ss) < ssize)
	_mpz_realloc (ss, ssize);
      sp = PTR (ss);
      MPN_COPY (sp, tmp_sp, ssize);
      SIZ (ss) = SIZ (&stmp);
    }

  if (ALLOC (g) < gsize)
    _mpz_realloc (g, gsize);
  gp = PTR (g);
  MPN_COPY (gp, tmp_gp, gsize);
  SIZ (g) = gsize;

  TMP_FREE (marker);
}
