/* mpz_addmul_ui(prodsum, multiplier, small_multiplicand) --
   Add MULTIPLICATOR times SMALL_MULTIPLICAND to PRODSUM.

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

static mp_limb_t mpn_neg1 _PROTO ((mp_ptr, mp_size_t));

#if 0
#undef  MPN_NORMALIZE
#define MPN_NORMALIZE(DST, NLIMBS) \
  do {									\
    while (--(NLIMBS) >= 0 && (DST)[NLIMBS] == 0)			\
      ;									\
    (NLIMBS)++;								\
  } while (0)
#undef  MPN_NORMALIZE_NOT_ZERO
#define MPN_NORMALIZE_NOT_ZERO(DST, NLIMBS) \
  do {									\
    while ((DST)[--(NLIMBS)] == 0)					\
      ;									\
    (NLIMBS)++;								\
  } while (0)
#endif

void
#if __STDC__
mpz_addmul_ui (mpz_ptr rz, mpz_srcptr az, unsigned long int bu)
#else
mpz_addmul_ui (rz, az, bu)
     mpz_ptr rz;
     mpz_srcptr az;
     unsigned long int bu;
#endif
{
  mp_size_t rn, an;
  mp_ptr rp, ap;

  an = SIZ (az);

  /* If either multiplier is zero, result is unaffected.  */
  if (bu == 0 || an == 0)
    return;

  rn = SIZ (rz);

  if (rn == 0)
    {
      mp_limb_t cy;

      an = ABS (an);
      if (ALLOC (rz) <= an)
	_mpz_realloc (rz, an + 1);
      rp = PTR (rz);
      ap = PTR (az);
      cy = mpn_mul_1 (rp, ap, an, (mp_limb_t) bu);
      rp[an] = cy;
      an += cy != 0;
      SIZ (rz) = SIZ (az) >= 0 ? an : -an;
      return;
    }

  if ((an ^ rn) >= 0)
    {
      /* Sign of operands are the same--really add.  */
      an = ABS (an);
      rn = ABS (rn);
      if (rn > an)
	{
	  mp_limb_t cy;
	  if (ALLOC (rz) <= rn)
	    _mpz_realloc (rz, rn + 1);
	  rp = PTR (rz);
	  ap = PTR (az);
	  cy = mpn_addmul_1 (rp, ap, an, (mp_limb_t) bu);
	  cy = mpn_add_1 (rp + an, rp + an, rn - an, cy);
	  rp[rn] = cy;
	  rn += cy != 0;
	  SIZ (rz) = SIZ (rz) >= 0 ? rn : -rn;
	  return;
	}
      else
	{
	  mp_limb_t cy;
	  if (ALLOC (rz) <= an)
	    _mpz_realloc (rz, an + 1);
	  rp = PTR (rz);
	  ap = PTR (az);
	  cy = mpn_addmul_1 (rp, ap, rn, (mp_limb_t) bu);
	  if (an != rn)
	    {
	      mp_limb_t cy2;
	      cy2 = mpn_mul_1 (rp + rn, ap + rn, an - rn, (mp_limb_t) bu);
	      cy = cy2 + mpn_add_1 (rp + rn, rp + rn, an - rn, cy);
	    }
	  rn = an;
	  rp[rn] = cy;
	  rn += cy != 0;
	  SIZ (rz) = SIZ (rz) >= 0 ? rn : -rn;
	  return;
	}
    }
  else
    {
      /* Sign of operands are different--actually subtract.  */
      an = ABS (an);
      rn = ABS (rn);
      if (rn > an)
	{
	  mp_limb_t cy;
	  rp = PTR (rz);
	  ap = PTR (az);
	  cy = mpn_submul_1 (rp, ap, an, (mp_limb_t) bu);
	  cy = mpn_sub_1 (rp + an, rp + an, rn - an, cy);
	  if (cy != 0)
	    {
	      mpn_neg1 (rp, rn);
	      MPN_NORMALIZE_NOT_ZERO (rp, rn);
	    }
	  else
	    {
	      MPN_NORMALIZE (rp, rn);
	      rn = -rn;
	    }

	  SIZ (rz) = SIZ (rz) >= 0 ? -rn : rn;
	  return;
	}
      else
	{
	  /* Tricky case.  We need to subtract an operand that might be larger
	     than the minuend.  To avoid allocating temporary space, we compute
	     a*b-r instead of r-a*b and then negate.  */
	  mp_limb_t cy;
	  if (ALLOC (rz) <= an)
	    _mpz_realloc (rz, an + 1);
	  rp = PTR (rz);
	  ap = PTR (az);
	  cy = mpn_submul_1 (rp, ap, rn, (mp_limb_t) bu);
	  if (an != rn)
	    {
	      mp_limb_t cy2;
	      cy -= mpn_neg1 (rp, rn);
	      cy2 = mpn_mul_1 (rp + rn, ap + rn, an - rn, (mp_limb_t) bu);
	      if (cy == ~(mp_limb_t) 0)
		cy = cy2 - mpn_sub_1 (rp + rn, rp + rn, an - rn, (mp_limb_t) 1);
	      else
		cy = cy2 + mpn_add_1 (rp + rn, rp + rn, an - rn, cy);
	      rp[an] = cy;
	      rn = an + (cy != 0);
	      rn -= rp[rn - 1] == 0;
	    }
	  else if (cy != 0)
	    {
	      cy -= mpn_neg1 (rp, rn);
	      rp[an] = cy;
	      rn = an + 1;
	      MPN_NORMALIZE_NOT_ZERO (rp, rn);
	    }
	  else
	    {
	      rn = an;
	      MPN_NORMALIZE (rp, rn);
	      rn = -rn;
	    }

	  SIZ (rz) = SIZ (rz) >= 0 ? -rn : rn;
	  return;
	}
    }
}

static mp_limb_t
#if __STDC__
mpn_neg1 (mp_ptr rp, mp_size_t rn)
#else
mpn_neg1 (rp, rn)
     mp_ptr rp;
     mp_size_t rn;
#endif
{
  mp_size_t i;

  while (rn != 0 && rp[0] == 0)
    rp++, rn--;

  if (rn != 0)
    {
      rp[0] = -rp[0];
      for (i = 1; i < rn; i++)
	rp[i] = ~rp[i];
      return 1;
    }
  return 0;
}
