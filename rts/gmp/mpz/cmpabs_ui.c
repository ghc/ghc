/* mpz_cmpabs_ui.c -- Compare a mpz_t a with an mp_limb_t b.  Return positive,
  zero, or negative based on if a > b, a == b, or a < b.

Copyright (C) 1991, 1993, 1994, 1995, 1997, 2000 Free Software Foundation,
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

int
#if __STDC__
mpz_cmpabs_ui (mpz_srcptr u, unsigned long int v_digit)
#else
mpz_cmpabs_ui (u, v_digit)
     mpz_srcptr u;
     unsigned long int v_digit;
#endif
{
  mp_size_t usize = u->_mp_size;

  if (usize == 0)
    return -(v_digit != 0);

  usize = ABS (usize);

  if (usize == 1)
    {
      mp_limb_t u_digit;

      u_digit = u->_mp_d[0];
      if (u_digit > v_digit)
	return 1;
      if (u_digit < v_digit)
	return -1;
      return 0;
    }

  return 1;
}
