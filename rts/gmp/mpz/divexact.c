/* mpz_divexact -- finds quotient when known that quot * den == num && den != 0.

Copyright (C) 1991, 1993, 1994, 1995, 1996, 1997, 1998, 2000 Free Software
Foundation, Inc.

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
MA 02111-1307, USA.  */

/*  Ken Weber (kweber@mat.ufrgs.br, kweber@mcs.kent.edu)

    Funding for this work has been partially provided by Conselho Nacional
    de Desenvolvimento Cienti'fico e Tecnolo'gico (CNPq) do Brazil, Grant
    301314194-2, and was done while I was a visiting reseacher in the Instituto
    de Matema'tica at Universidade Federal do Rio Grande do Sul (UFRGS).

    References:
	T. Jebelean, An algorithm for exact division, Journal of Symbolic
	Computation, v. 15, 1993, pp. 169-180.	*/

#include "gmp.h"
#include "gmp-impl.h"
#include "longlong.h"

void
#if __STDC__
mpz_divexact (mpz_ptr quot, mpz_srcptr num, mpz_srcptr den)
#else
mpz_divexact (quot, num, den)
     mpz_ptr quot;
     mpz_srcptr num;
     mpz_srcptr den;
#endif
{
  mp_ptr qp, tp;
  mp_size_t qsize, tsize;
  mp_srcptr np, dp;
  mp_size_t nsize, dsize;
  TMP_DECL (marker);

  nsize = ABS (num->_mp_size);
  dsize = ABS (den->_mp_size);

  qsize = nsize - dsize + 1;
  if (quot->_mp_alloc < qsize)
    _mpz_realloc (quot, qsize);

  np = num->_mp_d;
  dp = den->_mp_d;
  qp = quot->_mp_d;

  if (nsize == 0)
    {
      if (dsize == 0)
	DIVIDE_BY_ZERO;
      quot->_mp_size = 0;
      return;
    }

  if (dsize <= 1)
    {
      if (dsize == 1)
	{
	  mpn_divmod_1 (qp, np, nsize, dp[0]);
	  qsize -= qp[qsize - 1] == 0;
	  quot->_mp_size = (num->_mp_size ^ den->_mp_size) >= 0 ? qsize : -qsize;
	  return;
	}

      /*  Generate divide-by-zero error since dsize == 0.  */
      DIVIDE_BY_ZERO;
    }

  TMP_MARK (marker);

  /*  QUOT <-- NUM/2^r, T <-- DEN/2^r where = r number of twos in DEN.  */
  while (dp[0] == 0)
    np += 1, nsize -= 1, dp += 1, dsize -= 1;
  tsize = MIN (qsize, dsize);
  if ((dp[0] & 1) != 0)
    {
      if (quot == den)		/*  QUOT and DEN overlap.  */
	{
	  tp = (mp_ptr) TMP_ALLOC (tsize * BYTES_PER_MP_LIMB);
	  MPN_COPY (tp, dp, tsize);
	}
      else
	tp = (mp_ptr) dp;
      if (qp != np)
	MPN_COPY_INCR (qp, np, qsize);
    }
  else
    {
      unsigned int r;
      tp = (mp_ptr) TMP_ALLOC (tsize * BYTES_PER_MP_LIMB);
      count_trailing_zeros (r, dp[0]);
      mpn_rshift (tp, dp, tsize, r);
      if (dsize > tsize)
	tp[tsize - 1] |= dp[tsize] << (BITS_PER_MP_LIMB - r);
      mpn_rshift (qp, np, qsize, r);
      if (nsize > qsize)
	qp[qsize - 1] |= np[qsize] << (BITS_PER_MP_LIMB - r);
    }

  /*  Now QUOT <-- QUOT/T.  */
  mpn_bdivmod (qp, qp, qsize, tp, tsize, qsize * BITS_PER_MP_LIMB);
  MPN_NORMALIZE (qp, qsize);

  quot->_mp_size = (num->_mp_size ^ den->_mp_size) >= 0 ? qsize : -qsize;

  TMP_FREE (marker);
}
