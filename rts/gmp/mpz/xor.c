/* mpz_xor -- Logical xor.

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

void
#if __STDC__
mpz_xor (mpz_ptr res, mpz_srcptr op1, mpz_srcptr op2)
#else
mpz_xor (res, op1, op2)
     mpz_ptr res;
     mpz_srcptr op1;
     mpz_srcptr op2;
#endif
{
  mp_srcptr op1_ptr, op2_ptr;
  mp_size_t op1_size, op2_size;
  mp_ptr res_ptr;
  mp_size_t res_size, res_alloc;
  mp_size_t i;
  TMP_DECL (marker);

  TMP_MARK (marker);
  op1_size = op1->_mp_size;
  op2_size = op2->_mp_size;

  op1_ptr = op1->_mp_d;
  op2_ptr = op2->_mp_d;
  res_ptr = res->_mp_d;

  if (op1_size >= 0)
    {
      if (op2_size >= 0)
	{
	  if (op1_size >= op2_size)
	    {
	      if (res->_mp_alloc < op1_size)
		{
		  _mpz_realloc (res, op1_size);
		  op1_ptr = op1->_mp_d;
		  op2_ptr = op2->_mp_d;
		  res_ptr = res->_mp_d;
		}

	      if (res_ptr != op1_ptr)
		MPN_COPY (res_ptr + op2_size, op1_ptr + op2_size,
			  op1_size - op2_size);
	      for (i = op2_size - 1; i >= 0; i--)
		res_ptr[i] = op1_ptr[i] ^ op2_ptr[i];
	      res_size = op1_size;
	    }
	  else
	    {
	      if (res->_mp_alloc < op2_size)
		{
		  _mpz_realloc (res, op2_size);
		  op1_ptr = op1->_mp_d;
		  op2_ptr = op2->_mp_d;
		  res_ptr = res->_mp_d;
		}

	      if (res_ptr != op2_ptr)
		MPN_COPY (res_ptr + op1_size, op2_ptr + op1_size,
			  op2_size - op1_size);
	      for (i = op1_size - 1; i >= 0; i--)
		res_ptr[i] = op1_ptr[i] ^ op2_ptr[i];
	      res_size = op2_size;
	    }

	  MPN_NORMALIZE (res_ptr, res_size);
	  res->_mp_size = res_size;
	  return;
	}
      else /* op2_size < 0 */
	{
	  /* Fall through to the code at the end of the function.  */
	}
    }
  else
    {
      if (op2_size < 0)
	{
	  mp_ptr opx;
	  mp_limb_t cy;

	  /* Both operands are negative, the result will be positive.
	      (-OP1) ^ (-OP2) =
	     = ~(OP1 - 1) ^ ~(OP2 - 1) =
	     = (OP1 - 1) ^ (OP2 - 1)  */

	  op1_size = -op1_size;
	  op2_size = -op2_size;

	  /* Possible optimization: Decrease mpn_sub precision,
	     as we won't use the entire res of both.  */
	  opx = (mp_ptr) TMP_ALLOC (op1_size * BYTES_PER_MP_LIMB);
	  mpn_sub_1 (opx, op1_ptr, op1_size, (mp_limb_t) 1);
	  op1_ptr = opx;

	  opx = (mp_ptr) TMP_ALLOC (op2_size * BYTES_PER_MP_LIMB);
	  mpn_sub_1 (opx, op2_ptr, op2_size, (mp_limb_t) 1);
	  op2_ptr = opx;

	  res_alloc = MAX (op1_size, op2_size);
	  if (res->_mp_alloc < res_alloc)
	    {
	      _mpz_realloc (res, res_alloc);
	      res_ptr = res->_mp_d;
	      /* Don't re-read OP1_PTR and OP2_PTR.  They point to
		 temporary space--never to the space RES->_mp_d used
		 to point to before reallocation.  */
	    }

	  if (op1_size > op2_size)
	    {
	      MPN_COPY (res_ptr + op2_size, op1_ptr + op2_size,
			op1_size - op2_size);
	      for (i = op2_size - 1; i >= 0; i--)
		res_ptr[i] = op1_ptr[i] ^ op2_ptr[i];
	      res_size = op1_size;
	    }
	  else
	    {
	      MPN_COPY (res_ptr + op1_size, op2_ptr + op1_size,
			op2_size - op1_size);
	      for (i = op1_size - 1; i >= 0; i--)
		res_ptr[i] = op1_ptr[i] ^ op2_ptr[i];
	      res_size = op2_size;
	    }

	  MPN_NORMALIZE (res_ptr, res_size);
	  res->_mp_size = res_size;
	  TMP_FREE (marker);
	  return;
	}
      else
	{
	  /* We should compute -OP1 ^ OP2.  Swap OP1 and OP2 and fall
	     through to the code that handles OP1 ^ -OP2.  */
          MPZ_SRCPTR_SWAP (op1, op2);
          MPN_SRCPTR_SWAP (op1_ptr,op1_size, op2_ptr,op2_size);
	}
    }

  {
    mp_ptr opx;
    mp_limb_t cy;
    mp_size_t count;

    /* Operand 2 negative, so will be the result.
       -(OP1 ^ (-OP2)) = -(OP1 ^ ~(OP2 - 1)) =
       = ~(OP1 ^ ~(OP2 - 1)) + 1 =
       = (OP1 ^ (OP2 - 1)) + 1      */

    op2_size = -op2_size;

    opx = (mp_ptr) TMP_ALLOC (op2_size * BYTES_PER_MP_LIMB);
    mpn_sub_1 (opx, op2_ptr, op2_size, (mp_limb_t) 1);
    op2_ptr = opx;

    res_alloc = MAX (op1_size, op2_size) + 1;
    if (res->_mp_alloc < res_alloc)
      {
	_mpz_realloc (res, res_alloc);
	op1_ptr = op1->_mp_d;
	res_ptr = res->_mp_d;
	/* Don't re-read OP2_PTR.  It points to temporary space--never
	   to the space RES->_mp_d used to point to before reallocation.  */
      }

    if (op1_size > op2_size)
      {
	MPN_COPY (res_ptr + op2_size, op1_ptr + op2_size, op1_size - op2_size);
	for (i = op2_size - 1; i >= 0; i--)
	  res_ptr[i] = op1_ptr[i] ^ op2_ptr[i];
	res_size = op1_size;
      }
    else
      {
	MPN_COPY (res_ptr + op1_size, op2_ptr + op1_size, op2_size - op1_size);
	for (i = op1_size - 1; i >= 0; i--)
	  res_ptr[i] = op1_ptr[i] ^ op2_ptr[i];
	res_size = op2_size;
      }

    cy = mpn_add_1 (res_ptr, res_ptr, res_size, (mp_limb_t) 1);
    if (cy)
      {
	res_ptr[res_size] = cy;
	res_size++;
      }

    MPN_NORMALIZE (res_ptr, res_size);
    res->_mp_size = -res_size;
    TMP_FREE (marker);
  }
}
