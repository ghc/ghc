/* gmp_randclear (state) -- Clear and deallocate random state STATE.

Copyright (C) 1999, 2000  Free Software Foundation, Inc.

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
gmp_randclear (gmp_randstate_t rstate)
#else
gmp_randclear (rstate)
     gmp_randstate_t rstate;
#endif
{
  mpz_clear (rstate->seed);

  switch (rstate->alg)
    {
    case GMP_RAND_ALG_LC:
      mpz_clear (rstate->algdata.lc->a);
      if (rstate->algdata.lc->m2exp == 0)
	mpz_clear (rstate->algdata.lc->m);
      (*_mp_free_func) (rstate->algdata.lc, sizeof (*rstate->algdata.lc));
      break;

#if 0
    case GMP_RAND_ALG_BBS:
      mpz_clear (rstate->algdata.bbs->bi);
      (*_mp_free_func) (rstate->algdata.bbs, sizeof (*rstate->algdata.bbs));
      break;
#endif /* 0 */

    default:
      gmp_errno |= GMP_ERROR_UNSUPPORTED_ARGUMENT;
    }
}
