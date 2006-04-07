/* mpz_tdiv_q -- divide two integers and produce a quotient.

Copyright (C) 1991, 1993, 1994, 1996, 2000 Free Software Foundation, Inc.

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

void
#if __STDC__
mpz_tdiv_q (mpz_ptr quot, mpz_srcptr num, mpz_srcptr den)
#else
mpz_tdiv_q (quot, num, den)
     mpz_ptr quot;
     mpz_srcptr num;
     mpz_srcptr den;
#endif
{
  mp_size_t ql;
  mp_size_t ns, ds, nl, dl;
  mp_ptr np, dp, qp, rp;
  TMP_DECL (marker);

  ns = SIZ (num);
  ds = SIZ (den);
  nl = ABS (ns);
  dl = ABS (ds);
  ql = nl - dl + 1;

  if (dl == 0)
    DIVIDE_BY_ZERO;

  if (ql <= 0)
    {
      SIZ (quot) = 0;
      return;
    }

  MPZ_REALLOC (quot, ql);

  TMP_MARK (marker);
  qp = PTR (quot);
  rp = (mp_ptr) TMP_ALLOC (dl * BYTES_PER_MP_LIMB);
  np = PTR (num);
  dp = PTR (den);

  /* FIXME: We should think about how to handle the temporary allocation.
     Perhaps mpn_tdiv_qr should handle it, since it anyway often needs to
     allocate temp space.  */

  /* Copy denominator to temporary space if it overlaps with the quotient.  */
  if (dp == qp)
    {
      mp_ptr tp;
      tp = (mp_ptr) TMP_ALLOC (dl * BYTES_PER_MP_LIMB);
      MPN_COPY (tp, dp, dl);
      dp = tp;
    }
  /* Copy numerator to temporary space if it overlaps with the quotient.  */
  if (np == qp)
    {
      mp_ptr tp;
      tp = (mp_ptr) TMP_ALLOC (nl * BYTES_PER_MP_LIMB);
      MPN_COPY (tp, np, nl);
      np = tp;
    }

  mpn_tdiv_qr (qp, rp, 0L, np, nl, dp, dl);

  ql -=  qp[ql - 1] == 0;

  SIZ (quot) = (ns ^ ds) >= 0 ? ql : -ql;
  TMP_FREE (marker);
}
