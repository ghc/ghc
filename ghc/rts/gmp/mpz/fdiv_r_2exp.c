/* mpz_fdiv_r_2exp -- Divide a integer by 2**CNT and produce a remainder.

Copyright (C) 1991, 1993, 1994, 1995, 1998, 1999 Free Software Foundation,
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

void
#if __STDC__
mpz_fdiv_r_2exp (mpz_ptr res, mpz_srcptr in, unsigned long int cnt)
#else
mpz_fdiv_r_2exp (res, in, cnt)
     mpz_ptr res;
     mpz_srcptr in;
     unsigned long int cnt;
#endif
{
  mp_size_t in_size = ABS (in->_mp_size);
  mp_size_t res_size;
  mp_size_t limb_cnt = cnt / BITS_PER_MP_LIMB;
  mp_srcptr in_ptr = in->_mp_d;

  if (in_size > limb_cnt)
    {
      /* The input operand is (probably) greater than 2**CNT.  */
      mp_limb_t x;

      x = in_ptr[limb_cnt] & (((mp_limb_t) 1 << cnt % BITS_PER_MP_LIMB) - 1);
      if (x != 0)
	{
	  res_size = limb_cnt + 1;
	  if (res->_mp_alloc < res_size)
	    _mpz_realloc (res, res_size);

	  res->_mp_d[limb_cnt] = x;
	}
      else
	{
	  res_size = limb_cnt;
	  MPN_NORMALIZE (in_ptr, res_size);

	  if (res->_mp_alloc < res_size)
	    _mpz_realloc (res, res_size);

	  limb_cnt = res_size;
	}
    }
  else
    {
      /* The input operand is smaller than 2**CNT.  We perform a no-op,
	 apart from that we might need to copy IN to RES, and may need
	 to round the result.  */
      res_size = in_size;
      if (res->_mp_alloc < res_size)
	_mpz_realloc (res, res_size);

      limb_cnt = res_size;
    }

  if (res != in)
    MPN_COPY (res->_mp_d, in->_mp_d, limb_cnt);
  in_size = in->_mp_size;
  res->_mp_size = res_size;
  if (in_size < 0 && res_size != 0)
    {
      /* Result should be 2^CNT - RES */
      mpz_t tmp;
      MPZ_TMP_INIT (tmp, cnt/BITS_PER_MP_LIMB + 2);
      mpz_set_ui (tmp, 1L);
      mpz_mul_2exp (tmp, tmp, cnt);
      mpz_sub (res, tmp, res);
    }
}

/* This is an alternative ending of the above function using just low-level
   functions.  Tested, but perhaps excessive?  */
#if 0
  if (in->_mp_size < 0 && res_size != 0)
    {
      /* Result should be 2^CNT - RES */

      mp_ptr rp;

      limb_cnt = cnt / BITS_PER_MP_LIMB;

      if (res->_mp_alloc <= limb_cnt)
	_mpz_realloc (res, limb_cnt + 1);
      rp = PTR(res);
      if (res_size > limb_cnt)
	{
	  mpn_nz_neg (rp, rp, res_size);
	  rp[limb_cnt] &= ~(~(mp_limb_t) 0 << cnt % BITS_PER_MP_LIMB);
	  MPN_NORMALIZE_NOT_ZERO (rp, res_size);
	}
      else
	{
	  mp_size_t i;
	  mpn_nz_neg (rp, rp, res_size);
	  for (i = res_size; i < limb_cnt; i++)
	    rp[i] = ~ (mp_limb_t) 0;
	  res_size = limb_cnt;
	  if (cnt % BITS_PER_MP_LIMB != 0)
	    {
	      rp[res_size] = ((mp_limb_t) 1 << (cnt % BITS_PER_MP_LIMB)) - 1;
	      res_size++;
	    }
	  else
	    MPN_NORMALIZE_NOT_ZERO (rp, res_size);
	}
    }
  SIZ(res) = res_size;
}

static void
mpn_nz_neg (rp, sp, n)
     mp_ptr rp, sp;
     mp_size_t n;
{
  mp_size_t i;
  mp_limb_t x;

  x = sp[0];
  rp[0] = -x;
  for (i = 1; x == 0; i++)
    {
      x = sp[i];
      rp[i] = -x;
    }

  for (; i < n; i++)
    {
      rp[i] = ~sp[i];
    }
}
#endif
