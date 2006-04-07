/* mpn_perfect_square_p(u,usize) -- Return non-zero if U is a perfect square,
   zero otherwise.

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

#include <stdio.h> /* for NULL */
#include "gmp.h"
#include "gmp-impl.h"
#include "longlong.h"


/* sq_res_0x100[x mod 0x100] == 1 iff x mod 0x100 is a quadratic residue
   modulo 0x100.  */
static unsigned char const sq_res_0x100[0x100] =
{
  1,1,0,0,1,0,0,0,0,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,
  0,1,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,
  1,1,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,
  0,1,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,
  0,1,0,0,1,0,0,0,0,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,
  0,1,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,
  0,1,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,
  0,1,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,
};

int
#if __STDC__
mpn_perfect_square_p (mp_srcptr up, mp_size_t usize)
#else
mpn_perfect_square_p (up, usize)
     mp_srcptr up;
     mp_size_t usize;
#endif
{
  mp_limb_t rem;
  mp_ptr root_ptr;
  int res;
  TMP_DECL (marker);

  /* The first test excludes 55/64 (85.9%) of the perfect square candidates
     in O(1) time.  */
  if ((sq_res_0x100[(unsigned int) up[0] % 0x100] & 1) == 0)
    return 0;

#if defined (PP)
  /* The second test excludes 30652543/30808063 (99.5%) of the remaining
     perfect square candidates in O(n) time.  */

  /* Firstly, compute REM = A mod PP.  */
  if (UDIV_TIME > (2 * UMUL_TIME + 6))
    rem = mpn_preinv_mod_1 (up, usize, (mp_limb_t) PP, (mp_limb_t) PP_INVERTED);
  else
    rem = mpn_mod_1 (up, usize, (mp_limb_t) PP);

  /* Now decide if REM is a quadratic residue modulo the factors in PP.  */

  /* If A is just a few limbs, computing the square root does not take long
     time, so things might run faster if we limit this loop according to the
     size of A.  */

#if BITS_PER_MP_LIMB == 64
  if (((CNST_LIMB(0x12DD703303AED3) >> rem % 53) & 1) == 0)
    return 0;
  if (((CNST_LIMB(0x4351B2753DF) >> rem % 47) & 1) == 0)
    return 0;
  if (((CNST_LIMB(0x35883A3EE53) >> rem % 43) & 1) == 0)
    return 0;
  if (((CNST_LIMB(0x1B382B50737) >> rem % 41) & 1) == 0)
    return 0;
  if (((CNST_LIMB(0x165E211E9B) >> rem % 37) & 1) == 0)
    return 0;
  if (((CNST_LIMB(0x121D47B7) >> rem % 31) & 1) == 0)
    return 0;
#endif
  if (((0x13D122F3L >> rem % 29) & 1) == 0)
    return 0;
  if (((0x5335FL >> rem % 23) & 1) == 0)
    return 0;
  if (((0x30AF3L >> rem % 19) & 1) == 0)
    return 0;
  if (((0x1A317L >> rem % 17) & 1) == 0)
    return 0;
  if (((0x161BL >> rem % 13) & 1) == 0)
    return 0;
  if (((0x23BL >> rem % 11) & 1) == 0)
    return 0;
  if (((0x017L >> rem % 7) & 1) == 0)
    return 0;
  if (((0x13L >> rem % 5) & 1) == 0)
    return 0;
  if (((0x3L >> rem % 3) & 1) == 0)
    return 0;
#endif

  TMP_MARK (marker);

  /* For the third and last test, we finally compute the square root,
     to make sure we've really got a perfect square.  */
  root_ptr = (mp_ptr) TMP_ALLOC ((usize + 1) / 2 * BYTES_PER_MP_LIMB);

  /* Iff mpn_sqrtrem returns zero, the square is perfect.  */
  res = ! mpn_sqrtrem (root_ptr, NULL, up, usize);
  TMP_FREE (marker);
  return res;
}
