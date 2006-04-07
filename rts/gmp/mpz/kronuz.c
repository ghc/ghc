/* mpz_ui_kronecker -- Kronecker/Jacobi symbol. */

/*
Copyright (C) 1999, 2000 Free Software Foundation, Inc.

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
MA 02111-1307, USA.
*/

#include "gmp.h"
#include "gmp-impl.h"
#include "longlong.h"


int
#if __STDC__
mpz_ui_kronecker (unsigned long a, mpz_srcptr b)
#else
mpz_ui_kronecker (a, b)
     unsigned long a;
     mpz_srcptr    b;
#endif
{
  int        b_abs_size;
  mp_srcptr  b_ptr;
  mp_limb_t  b_low;
  int        twos;
  int        result_bit1;

  /* (a/0) */
  b_abs_size = ABSIZ (b);
  if (b_abs_size == 0)
    return JACOBI_U0 (a);

  /* (a/-1)=1 when a>=0, so the sign of b is ignored */
  b_ptr = PTR(b);
  b_low = b_ptr[0];

  /* (0/1)=1; (0/-1)=1; (0/b)=0 for b!=+/-1
     (1/b)=1, for any b */
  if (a <= 1)
    return (a == 1) | ((b_abs_size == 1) & (b_low == 1));

  if (b_low & 1)
    {
      /* (a/1) = 1 for any a */
      if (b_abs_size == 1 && b_low == 1)
        return 1;

      count_trailing_zeros (twos, a);
      a >>= twos;
      if (a == 1)
        return JACOBI_TWOS_U (twos, b_low);  /* powers of (2/b) only */

      /* powers of (2/b); reciprocity to (b/a); (b/a) == (b mod a / a) */
      return mpn_jacobi_base (mpn_mod_1 (b_ptr, b_abs_size, a),
                              a,
                              JACOBI_TWOS_U_BIT1 (twos, b_low)
                              ^ JACOBI_RECIP_UU_BIT1 (b_low, a));
    }

  /* b is even; (a/2)=0 if a is even */
  if ((a & 1) == 0)
    return 0;

  /* Require MP_BITS_PER_LIMB even, so (a/2)^MP_BITS_PER_LIMB = 1, and so we
     don't have to pay attention to how many trailing zero limbs are
     stripped.  */
  ASSERT ((BITS_PER_MP_LIMB & 1) == 0);

  MPN_STRIP_LOW_ZEROS_NOT_ZERO (b_ptr, b_abs_size);
  b_low = b_ptr[0];

  if (b_low & 1)
    /* reciprocity to (b/a); (b/a) == (b mod a / a) */
    return mpn_jacobi_base (mpn_mod_1 (b_ptr, b_abs_size, a),
                            a,
                            JACOBI_RECIP_UU_BIT1 (b_low, a));

  count_trailing_zeros (twos, b_low);

  /* reciprocity to get (b/a) */
  if (twos == BITS_PER_MP_LIMB-1)
    {
      if (b_abs_size == 1)
        {
          /* b==0x800...00, one limb high bit only, so (a/2)^(BPML-1) */
          return JACOBI_TWOS_U (BITS_PER_MP_LIMB-1, a);
        }

      /* b_abs_size > 1 */
      result_bit1 = JACOBI_RECIP_UU_BIT1 (a, b_ptr[1] << 1);
    }
  else
    result_bit1 = JACOBI_RECIP_UU_BIT1 (a, b_low >> twos);

  /* powers of (a/2); reciprocity to (b/a); (b/a) == (b mod a / a) */
  return mpn_jacobi_base (mpn_mod_1_rshift (b_ptr, b_abs_size, twos, a),
                          a,
                          JACOBI_TWOS_U_BIT1 (twos, a) ^ result_bit1);
}
