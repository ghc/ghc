/* mpn_mul -- Multiply two natural numbers.

   THE HELPER FUNCTIONS IN THIS FILE (meaning everything except mpn_mul)
   ARE INTERNAL FUNCTIONS WITH MUTABLE INTERFACES.  IT IS ONLY SAFE TO REACH
   THEM THROUGH DOCUMENTED INTERFACES.  IN FACT, IT IS ALMOST GUARANTEED
   THAT THEY'LL CHANGE OR DISAPPEAR IN A FUTURE GNU MP RELEASE.


Copyright (C) 1991, 1993, 1994, 1996, 1997, 1999, 2000 Free Software
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

#include "gmp.h"
#include "gmp-impl.h"

/* Multiply the natural numbers u (pointed to by UP, with UN limbs) and v
   (pointed to by VP, with VN limbs), and store the result at PRODP.  The
   result is UN + VN limbs.  Return the most significant limb of the result.

   NOTE: The space pointed to by PRODP is overwritten before finished with U
   and V, so overlap is an error.

   Argument constraints:
   1. UN >= VN.
   2. PRODP != UP and PRODP != VP, i.e. the destination must be distinct from
      the multiplier and the multiplicand.  */

void
#if __STDC__
mpn_sqr_n (mp_ptr prodp,
         mp_srcptr up, mp_size_t un)
#else
mpn_sqr_n (prodp, up, un)
     mp_ptr prodp;
     mp_srcptr up;
     mp_size_t un;
#endif
{
  if (un < KARATSUBA_SQR_THRESHOLD)
    { /* plain schoolbook multiplication */
      if (un == 0)
	return;
      mpn_sqr_basecase (prodp, up, un);
    }
  else if (un < TOOM3_SQR_THRESHOLD)
    { /* karatsuba multiplication */
      mp_ptr tspace;
      TMP_DECL (marker);
      TMP_MARK (marker);
      tspace = (mp_ptr) TMP_ALLOC (2 * (un + BITS_PER_MP_LIMB) * BYTES_PER_MP_LIMB);
      mpn_kara_sqr_n (prodp, up, un, tspace);
      TMP_FREE (marker);
    }
#if WANT_FFT || TUNE_PROGRAM_BUILD
  else if (un < FFT_SQR_THRESHOLD)
#else
  else
#endif
    { /* toom3 multiplication */
      mp_ptr tspace;
      TMP_DECL (marker);
      TMP_MARK (marker);
      tspace = (mp_ptr) TMP_ALLOC (2 * (un + BITS_PER_MP_LIMB) * BYTES_PER_MP_LIMB);
      mpn_toom3_sqr_n (prodp, up, un, tspace);
      TMP_FREE (marker);
    }
#if WANT_FFT || TUNE_PROGRAM_BUILD
  else
    {
      /* schoenhage multiplication */
      mpn_mul_fft_full (prodp, up, un, up, un);
    }
#endif
}

mp_limb_t
#if __STDC__
mpn_mul (mp_ptr prodp,
	 mp_srcptr up, mp_size_t un,
	 mp_srcptr vp, mp_size_t vn)
#else
mpn_mul (prodp, up, un, vp, vn)
     mp_ptr prodp;
     mp_srcptr up;
     mp_size_t un;
     mp_srcptr vp;
     mp_size_t vn;
#endif
{
  mp_size_t l;
  mp_limb_t c;

  if (up == vp && un == vn)
    {
      mpn_sqr_n (prodp, up, un);
      return prodp[2 * un - 1];
    }

  if (vn < KARATSUBA_MUL_THRESHOLD)
    { /* long multiplication */
      mpn_mul_basecase (prodp, up, un, vp, vn);
      return prodp[un + vn - 1];
    }

  mpn_mul_n (prodp, up, vp, vn);
  if (un != vn)
    { mp_limb_t t;
      mp_ptr ws;
      TMP_DECL (marker);
      TMP_MARK (marker);

      prodp += vn;
      l = vn;
      up += vn;
      un -= vn;

      if (un < vn) 
	{
	  /* Swap u's and v's. */
          MPN_SRCPTR_SWAP (up,un, vp,vn);
	}

      ws = (mp_ptr) TMP_ALLOC (((vn >= KARATSUBA_MUL_THRESHOLD ? vn : un) + vn)
			       * BYTES_PER_MP_LIMB);

      t = 0;
      while (vn >= KARATSUBA_MUL_THRESHOLD)
	{
	  mpn_mul_n (ws, up, vp, vn);
	  if (l <= 2*vn) 
	    {
	      t += mpn_add_n (prodp, prodp, ws, l);
	      if (l != 2*vn)
		{
		  t = mpn_add_1 (prodp + l, ws + l, 2*vn - l, t);
		  l = 2*vn;
		}
	    }
	  else
	    {
	      c = mpn_add_n (prodp, prodp, ws, 2*vn);
	      t += mpn_add_1 (prodp + 2*vn, prodp + 2*vn, l - 2*vn, c);
	    }
	  prodp += vn;
	  l -= vn;
	  up += vn;
	  un -= vn;
	  if (un < vn) 
	    {
	      /* Swap u's and v's. */
              MPN_SRCPTR_SWAP (up,un, vp,vn);
	    }
	}

      if (vn)
	{
	  mpn_mul_basecase (ws, up, un, vp, vn);
	  if (l <= un + vn) 
	    {
	      t += mpn_add_n (prodp, prodp, ws, l);
	      if (l != un + vn)
		t = mpn_add_1 (prodp + l, ws + l, un + vn - l, t);
	    } 
	  else
	    {
	      c = mpn_add_n (prodp, prodp, ws, un + vn);
	      t += mpn_add_1 (prodp + un + vn, prodp + un + vn, l - un - vn, c);
	    }
	}

    TMP_FREE (marker);
  }
  return prodp[un + vn - 1];
}
