/* int mpz_fits_X_p (mpz_t src) -- Return whether src fits the C type X.

Copyright (C) 1997, 2000 Free Software Foundation, Inc.

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
mpz_fits_ushort_p (mpz_srcptr src)
#else
mpz_fits_ushort_p (src)
     mpz_srcptr src;
#endif
{
  mp_size_t size;
  mp_limb_t mpl;

  mpl = PTR(src)[0];
  size = SIZ(src);
  if (size < 0 || size > 1)
    return 0;
  return mpl <= ((unsigned short int) ~(unsigned int) 0);
}
