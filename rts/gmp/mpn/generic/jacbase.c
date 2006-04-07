/* mpn_jacobi_base -- limb/limb Jacobi symbol with restricted arguments.

   THIS INTERFACE IS PRELIMINARY AND MIGHT DISAPPEAR OR BE SUBJECT TO
   INCOMPATIBLE CHANGES IN A FUTURE RELEASE OF GMP. */

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
MA 02111-1307, USA.  */

#include "gmp.h"
#include "gmp-impl.h"
#include "longlong.h"


#if COUNT_TRAILING_ZEROS_TIME <= 7
/* If count_trailing_zeros is fast, use it.
   K7 at 7 cycles and P6 at 2 are good here.  K6 at 12-27 and P5 at 18-42
   are not.  The default 15 in longlong.h is meant to mean not good here.  */

#define PROCESS_TWOS_ANY                                \
  {                                                     \
    mp_limb_t  twos;                                    \
    count_trailing_zeros (twos, a);                     \
    result_bit1 ^= JACOBI_TWOS_U_BIT1 (twos, b);        \
    a >>= twos;                                         \
  }

#define PROCESS_TWOS_EVEN  PROCESS_TWOS_ANY

#else
/* Use a loop instead.  With "a" uniformly distributed there will usually be
   only a few trailing zeros.

   Unfortunately the branch for the while loop here will be on a 50/50
   chance of a 1 or 0, which is bad for branch prediction.  */

#define PROCESS_TWOS_EVEN               \
  {                                     \
    int  two;                           \
    two = JACOBI_TWO_U_BIT1 (b);        \
    do                                  \
      {                                 \
        a >>= 1;                        \
        result_bit1 ^= two;             \
        ASSERT (a != 0);                \
      }                                 \
    while ((a & 1) == 0);               \
  }

#define PROCESS_TWOS_ANY        \
  if ((a & 1) == 0)             \
    PROCESS_TWOS_EVEN;

#endif


/* Calculate the value of the Jacobi symbol (a/b) of two mp_limb_t's, but
   with a restricted range of inputs accepted, namely b>1, b odd, and a<=b.

   The initial result_bit1 is taken as a parameter for the convenience of
   mpz_kronecker_zi_ui() et al.  The sign changes both here and in those
   routines accumulate nicely in bit 1, see the JACOBI macros.

   The return value here is the normal +1, 0, or -1.  Note that +1 and -1
   have bit 1 in the "BIT1" sense, which could be useful if the caller is
   accumulating it into some extended calculation.

   Duplicating the loop body to avoid the MP_LIMB_T_SWAP(a,b) would be
   possible, but a couple of tests suggest it's not a significant speedup,
   and may even be a slowdown, so what's here is good enough for now.

   Future: The code doesn't demand a<=b actually, so maybe this could be
   relaxed.  All the places this is used currently call with a<=b though.  */

int
#if __STDC__
mpn_jacobi_base (mp_limb_t a, mp_limb_t b, int result_bit1)
#else
mpn_jacobi_base (a, b, result_bit1)
     mp_limb_t a;
     mp_limb_t b;
     int       result_bit1;
#endif
{
  ASSERT (b & 1);  /* b odd */
  ASSERT (b != 1);
  ASSERT (a <= b);

  if (a == 0)
    return 0;

  PROCESS_TWOS_ANY;
  if (a == 1)
    goto done;

  for (;;)
    {
      result_bit1 ^= JACOBI_RECIP_UU_BIT1 (a, b);
      MP_LIMB_T_SWAP (a, b);

      do
	{
          /* working on (a/b), a,b odd, a>=b */
          ASSERT (a & 1);
          ASSERT (b & 1);
          ASSERT (a >= b);

	  if ((a -= b) == 0)
	    return 0;

          PROCESS_TWOS_EVEN;
	  if (a == 1)
	    goto done;
	}
      while (a >= b);
    }

 done:
  return JACOBI_BIT1_TO_PN (result_bit1);
}
