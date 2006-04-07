/* mpz_dump - Dump an integer to stdout.

   THIS IS AN INTERNAL FUNCTION WITH A MUTABLE INTERFACE.  IT IS NOT SAFE TO
   CALL THIS FUNCTION DIRECTLY.  IN FACT, IT IS ALMOST GUARANTEED THAT THIS
   FUNCTION WILL CHANGE OR DISAPPEAR IN A FUTURE GNU MP RELEASE.


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
MA 02111-1307, USA. */

#include <stdio.h>
#include "gmp.h"
#include "gmp-impl.h"

void
#if __STDC__
mpz_dump (mpz_srcptr u)
#else
mpz_dump (u)
     mpz_srcptr u;
#endif
{
  char *str;

  str = mpz_get_str (0, 10, u);
  printf ("%s\n", str);
  (*_mp_free_func) (str, 0);/* ??? broken alloc interface, pass what size ??? */
}
