/* mpn_divrem -- Divide natural numbers, producing both remainder and
   quotient.  This is now just a middle layer for calling the new
   internal mpn_tdiv_qr.

Copyright (C) 1993, 1994, 1995, 1996, 1997, 1999, 2000 Free Software
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
MA 02111-1307, USA. */

#include "gmp.h"
#include "gmp-impl.h"
#include "longlong.h"

mp_limb_t
#if __STDC__
mpn_divrem (mp_ptr qp, mp_size_t qxn,
	    mp_ptr np, mp_size_t nn,
	    mp_srcptr dp, mp_size_t dn)
#else
mpn_divrem (qp, qxn, np, nn, dp, dn)
     mp_ptr qp;
     mp_size_t qxn;
     mp_ptr np;
     mp_size_t nn;
     mp_srcptr dp;
     mp_size_t dn;
#endif
{
  if (dn == 1)
    {
      mp_limb_t ret;
      mp_ptr q2p;
      mp_size_t qn;
      TMP_DECL (marker);

      TMP_MARK (marker);
      q2p = (mp_ptr) TMP_ALLOC ((nn + qxn) * BYTES_PER_MP_LIMB);

      np[0] = mpn_divrem_1 (q2p, qxn, np, nn, dp[0]);
      qn = nn + qxn - 1;
      MPN_COPY (qp, q2p, qn);
      ret = q2p[qn];

      TMP_FREE (marker);
      return ret;
    }
  else if (dn == 2)
    {
      return mpn_divrem_2 (qp, qxn, np, nn, dp);
    }
  else
    {
      mp_ptr rp, q2p;
      mp_limb_t qhl;
      mp_size_t qn;
      TMP_DECL (marker);

      TMP_MARK (marker);
      if (qxn != 0)
	{
	  mp_ptr n2p;
	  n2p = (mp_ptr) TMP_ALLOC ((nn + qxn) * BYTES_PER_MP_LIMB);
	  MPN_ZERO (n2p, qxn);
	  MPN_COPY (n2p + qxn, np, nn);
	  q2p = (mp_ptr) TMP_ALLOC ((nn - dn + qxn + 1) * BYTES_PER_MP_LIMB);
	  rp = (mp_ptr) TMP_ALLOC (dn * BYTES_PER_MP_LIMB);
	  mpn_tdiv_qr (q2p, rp, 0L, n2p, nn + qxn, dp, dn);
	  MPN_COPY (np, rp, dn);
	  qn = nn - dn + qxn;
	  MPN_COPY (qp, q2p, qn);
	  qhl = q2p[qn];
	}
      else
	{
	  q2p = (mp_ptr) TMP_ALLOC ((nn - dn + 1) * BYTES_PER_MP_LIMB);
	  rp = (mp_ptr) TMP_ALLOC (dn * BYTES_PER_MP_LIMB);
	  mpn_tdiv_qr (q2p, rp, 0L, np, nn, dp, dn);
	  MPN_COPY (np, rp, dn);	/* overwrite np area with remainder */
	  qn = nn - dn;
	  MPN_COPY (qp, q2p, qn);
	  qhl = q2p[qn];
	}
      TMP_FREE (marker);
      return qhl;
    }
}
