/* mpz/lcm.c:   Calculate the least common multiple of two integers.

Copyright (C) 1996 Free Software Foundation, Inc.

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

void *_mpz_realloc ();

void
#if __STDC__
mpz_lcm (mpz_ptr r, mpz_srcptr u, mpz_srcptr v)
#else
mpz_lcm (r, u, v)
     mpz_ptr r;
     mpz_srcptr u;
     mpz_srcptr v;
#endif
{
  mpz_t g;
  mp_size_t usize, vsize, size;

  usize = ABS (SIZ (u));
  vsize = ABS (SIZ (v));

  if (usize == 0 || vsize == 0)
    {
      SIZ (r) = 0;
      return;
    }

  size = MAX (usize, vsize);
  MPZ_TMP_INIT (g, size);

  mpz_gcd (g, u, v);
  mpz_divexact (g, u, g);
  mpz_mul (r, g, v);
}
