/* mpz_ui_pow_ui(res, base, exp) -- Set RES to BASE**EXP.

Copyright (C) 1991, 1993, 1994, 1996, 1997, 2000 Free Software Foundation,
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


static void mpz_pow2 _PROTO ((mpz_ptr r, mp_limb_t blimb, unsigned long int e, mp_limb_t rl));

void
#if __STDC__
mpz_ui_pow_ui (mpz_ptr r, unsigned long int b, unsigned long int e)
#else
mpz_ui_pow_ui (r, b, e)
     mpz_ptr r;
     unsigned long int b;
     unsigned long int e;
#endif
{
  mp_limb_t blimb = b;
  mp_limb_t rl;

  if (e == 0)
    {
      /* For x^0 we return 1, even if x is 0.  */
      r->_mp_d[0] = 1;
      r->_mp_size = 1;
      return;
    }

  /* Compute b^e as (b^n)^(e div n) * b^(e mod n), where n is chosen such that
     the latter factor is the largest number small enough to fit in a limb.  */

  rl = 1;
  while (e != 0 && blimb < ((mp_limb_t) 1 << BITS_PER_MP_LIMB/2))
    {
      if ((e & 1) != 0)
	rl = rl * blimb;
      blimb = blimb * blimb;
      e = e >> 1;
    }

  /* rl is now b^(e mod n).  (I.e., the latter factor above.)  */

  if (e == 0)
    {
      r->_mp_d[0] = rl;
      r->_mp_size = rl != 0;
      return;
    }

  mpz_pow2 (r, blimb, e, rl);
}

/* Multi-precision part of expontialization code.  */
static void
#if __STDC__
mpz_pow2 (mpz_ptr r, mp_limb_t blimb, unsigned long int e, mp_limb_t rl)
#else
mpz_pow2 (r, blimb, e, rl)
     mpz_ptr r;
     mp_limb_t blimb;
     unsigned long int e;
     mp_limb_t rl;
#endif
{
  mp_ptr rp, tp;
  mp_size_t ralloc, rsize;
  int cnt, i;
  TMP_DECL (marker);

  TMP_MARK (marker);

  /* Over-estimate temporary space requirements somewhat.  */
  count_leading_zeros (cnt, blimb);
  ralloc = e - cnt * e / BITS_PER_MP_LIMB + 1;

  /* The two areas are used to alternatingly hold the input and receive the
     product for mpn_mul.  (Needed since mpn_mul_n requires that the product
     is distinct from either input operand.)  */
  rp = (mp_ptr) TMP_ALLOC (ralloc * BYTES_PER_MP_LIMB);
  tp = (mp_ptr) TMP_ALLOC (ralloc * BYTES_PER_MP_LIMB);

  rp[0] = blimb;
  rsize = 1;

  count_leading_zeros (cnt, e);
  for (i = BITS_PER_MP_LIMB - cnt - 2; i >= 0; i--)
    {
      mpn_mul_n (tp, rp, rp, rsize);
      rsize = 2 * rsize;
      rsize -= tp[rsize - 1] == 0;
      MP_PTR_SWAP (rp, tp);

      if ((e & ((mp_limb_t) 1 << i)) != 0)
	{
	  mp_limb_t cy;
	  cy = mpn_mul_1 (rp, rp, rsize, blimb);
	  rp[rsize] = cy;
	  rsize += cy != 0;
	}
    }

  /* We will need rsize or rsize+1 limbs for the result.  */
  if (r->_mp_alloc <= rsize)
    _mpz_realloc (r, rsize + 1);

  /* Multiply the two factors (in rp,rsize and rl) and put the final result
     in place.  */
  {
    mp_limb_t cy;
    cy = mpn_mul_1 (r->_mp_d, rp, rsize, rl);
    (r->_mp_d)[rsize] = cy;
    rsize += cy != 0;
  }

  r->_mp_size = rsize;
  TMP_FREE (marker);
}
