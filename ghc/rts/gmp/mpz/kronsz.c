/* mpz_si_kronecker -- Kronecker/Jacobi symbol. */

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
mpz_si_kronecker (long a, mpz_srcptr b)
#else
mpz_si_kronecker (a, b)
     long       a;
     mpz_srcptr b;
#endif
{
  int        b_abs_size;
  mp_srcptr  b_ptr;
  mp_limb_t  b_low;
  int        twos;
  int        result_bit1;

  b_abs_size = ABSIZ (b);
  if (b_abs_size == 0)
    return JACOBI_S0 (a);  /* (a/0) */

  b_ptr = PTR(b);
  b_low = b_ptr[0];

  /* (0/b) = 1 if b=+/-1, 0 otherwise */
  if (a == 0)
    return (b_abs_size == 1) & (b_low == 1);

  /* account for the effect of the sign of b, so can then ignore it */
  result_bit1 = JACOBI_BSGN_SZ_BIT1 (a, b);

  if ((b_low & 1) == 0)
    {
      /* b even */

      if ((a & 1) == 0)
        return 0;  /* (a/b)=0 if both a,b even */

      /* Require MP_BITS_PER_LIMB even, so that (a/2)^MP_BITS_PER_LIMB = 1,
         and so that therefore there's no need to account for how many zero
         limbs are stripped.  */
      ASSERT ((BITS_PER_MP_LIMB & 1) == 0);

      MPN_STRIP_LOW_ZEROS_NOT_ZERO (b_ptr, b_abs_size);
      b_low = b_ptr[0];

      if ((b_low & 1) == 0)
        {
          /* odd a, even b */

          mp_limb_t  b_shl_bit1;

          count_trailing_zeros (twos, b_low);

          /* b_shl_bit1 is b>>twos, but with only bit 1 guaranteed */
          if (twos == BITS_PER_MP_LIMB-1)
            b_shl_bit1 = (b_abs_size == 1) ? 0 : (b_ptr[1] << 1);
          else
            b_shl_bit1 = (b_low >> twos);

          result_bit1 ^= JACOBI_ASGN_SU_BIT1 (a, b_shl_bit1);
          a = ABS(a);

          if (a == 1)
            return JACOBI_BIT1_TO_PN (result_bit1);  /* (1/b)=1 */

          /* twos (a/2), reciprocity to (b/a), and (b/a) = (b mod a / b) */
          return mpn_jacobi_base (mpn_mod_1_rshift (b_ptr, b_abs_size,
                                                    twos, a),
                                  a,
                                  result_bit1
                                  ^ JACOBI_TWOS_U_BIT1 (twos, a)
                                  ^ JACOBI_RECIP_UU_BIT1 (a, b_shl_bit1));
        }
    }

  /* b odd */

  result_bit1 ^= JACOBI_ASGN_SU_BIT1 (a, b_low);
  a = ABS(a);

  /* (a/1) = 1 for any a */
  if (b_abs_size == 1 && b_low == 1)
    return JACOBI_BIT1_TO_PN (result_bit1);

  /* Note a is cast to unsigned because 0x80..00 doesn't fit in a signed. */
  if ((a & 1) == 0)
    {
      count_trailing_zeros (twos, a);
      a = ((unsigned long) a) >> twos;
      result_bit1 ^= JACOBI_TWOS_U_BIT1 (twos, b_low);
    }

  if (a == 1)
    return JACOBI_BIT1_TO_PN (result_bit1);  /* (1/b)=1 */

  /* reciprocity to (b/a), and (b/a) == (b mod a / a) */
  return mpn_jacobi_base (mpn_mod_1 (b_ptr, b_abs_size, a), a,
                          result_bit1 ^ JACOBI_RECIP_UU_BIT1 (a, b_low));
}
