/* mpz_cmpabs(u,v) -- Compare U, V.  Return postive, zero, or negative
   based on if U > V, U == V, or U < V.

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

int
#if __STDC__
mpz_cmpabs (mpz_srcptr u, mpz_srcptr v)
#else
mpz_cmpabs (u, v)
     mpz_srcptr u;
     mpz_srcptr v;
#endif
{
  mp_size_t usize = u->_mp_size;
  mp_size_t vsize = v->_mp_size;
  mp_size_t size;
  mp_srcptr up, vp;
  int cmp;

  usize = ABS (usize);
  vsize = ABS (vsize);

  if (usize != vsize)
    return usize - vsize;

  if (usize == 0)
    return 0;

  up = u->_mp_d;
  vp = v->_mp_d;

  cmp = mpn_cmp (up, vp, usize);

  return cmp;
}
