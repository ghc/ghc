/* mpz_kronecker_ui -- Kronecker/Jacobi symbol. */

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


/* This function is expected to be often used with b an odd prime, so the
   code for odd b is nice and short. */

int
#if __STDC__
mpz_kronecker_ui (mpz_srcptr a, unsigned long b)
#else
mpz_kronecker_ui (a, b)
     mpz_srcptr    a;
     unsigned long b;
#endif
{
  int  twos;

  if (b & 1)
    {
      if (b != 1)
        return mpn_jacobi_base (mpz_fdiv_ui (a, b), b, 0);
      else
        return 1;  /* (a/1)=1 for any a */
    }

  if (b == 0)
    return JACOBI_Z0 (a);

  /* (a/2)=0 if a even */
  if (mpz_even_p (a))
    return 0;

  /* (a/2)=(2/a) when a odd */
  count_trailing_zeros (twos, b);  
  b >>= twos;
  if (b == 1)
    return JACOBI_TWOS_U (twos, PTR(a)[0]);

  return mpn_jacobi_base (mpz_fdiv_ui (a, b), b,
                          JACOBI_TWOS_U_BIT1(twos, PTR(a)[0]));
}
