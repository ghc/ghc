/* mpz_kronecker_si -- Kronecker/Jacobi symbol. */

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


/* This function is expected to be often used with b odd, so there's a test
   for this before invoking count_trailing_zeros().

   After the absolute value of b is established it's treated as an unsigned
   long, because 0x80..00 doesn't fit in a signed long. */

int
#if __STDC__
mpz_kronecker_si (mpz_srcptr a, long b)
#else
mpz_kronecker_si (a, b)
     mpz_srcptr a;
     long       b;
#endif
{
  int  result_bit1;
  int  twos;

  if (b == 0)
    return JACOBI_Z0 (a);

  result_bit1 = JACOBI_BSGN_ZS_BIT1(a, b);
  b = ABS (b);

  if (b == 1)
    return JACOBI_BIT1_TO_PN (result_bit1);  /* (a/1) = 1 for any a */

  if (b & 1) 
    return mpn_jacobi_base (mpz_fdiv_ui (a, b), b, result_bit1);
      
  /* result 0 if both a,b even */
  if (mpz_even_p (a))
    return 0;

  /* (a/2)=(2/a) when a odd */
  count_trailing_zeros (twos, b);
  result_bit1 ^= JACOBI_TWOS_U_BIT1 (twos, PTR(a)[0]);

  b = ((unsigned long) b) >> twos;
  if (b == 1)
    return JACOBI_BIT1_TO_PN (result_bit1);
  else
    return mpn_jacobi_base (mpz_fdiv_ui (a, b), b, result_bit1);
}


