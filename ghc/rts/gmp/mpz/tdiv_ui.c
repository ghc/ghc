/* mpz_tdiv_ui(dividend, divisor_limb)
   -- Return DIVDEND mod DIVISOR_LIMB.

Copyright (C) 1991, 1993, 1994, 1996, 1997, 1998 Free Software Foundation,
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

unsigned long int
#if __STDC__
mpz_tdiv_ui (mpz_srcptr dividend, unsigned long int divisor)
#else
mpz_tdiv_ui (dividend, divisor)
     mpz_srcptr dividend;
     unsigned long int divisor;
#endif
{
  mp_size_t dividend_size;
  mp_size_t size;
  mp_limb_t remainder_limb;

  if (divisor == 0)
    DIVIDE_BY_ZERO;

  dividend_size = dividend->_mp_size;
  size = ABS (dividend_size);

  /* No need for temporary allocation and copying if QUOT == DIVIDEND as
     the divisor is just one limb, and thus no intermediate remainders
     need to be stored.  */

  remainder_limb = mpn_mod_1 (dividend->_mp_d, size, (mp_limb_t) divisor);

  return remainder_limb;
}
