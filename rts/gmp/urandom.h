/* urandom.h -- define urandom returning a full unsigned long random value.

Copyright (C) 1995, 1996, 1997, 2000 Free Software Foundation, Inc.

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

#if defined (__hpux) || defined (__svr4__) || defined (__SVR4)
/* HPUX lacks random().  */
static inline mp_limb_t
urandom ()
{
  return mrand48 ();
}
#define __URANDOM
#endif

#if defined(_WIN32) && !(defined(__CYGWIN__) || defined(__CYGWIN32__))
/* MS CRT supplies just the poxy rand(), with an upper bound of 0x7fff */
static inline unsigned long
urandom ()
{
  return rand () ^ (rand () << 16) ^ (rand() << 32);
}
#define __URANDOM
#endif

#if defined (__alpha) && !defined (__URANDOM)
/* DEC OSF/1 1.2 random() returns a double.  */
long mrand48 ();
static inline mp_limb_t
urandom ()
{
  return mrand48 () | (mrand48 () << 32);
}
#define __URANDOM
#endif

#if BITS_PER_MP_LIMB == 32 && !defined (__URANDOM)
#if defined (__cplusplus)
extern "C" {
#endif
long random ();
#if defined (__cplusplus)
}
#endif
static inline mp_limb_t
urandom ()
{
  /* random() returns 31 bits, we want 32.  */
  return random () ^ (random () << 1);
}
#define __URANDOM
#endif

#if BITS_PER_MP_LIMB == 64 && !defined (__URANDOM)
#if defined (__cplusplus)
extern "C" {
#endif
long random ();
#if defined (__cplusplus)
}
#endif
static inline mp_limb_t
urandom ()
{
  /* random() returns 31 bits, we want 64.  */
  return random () ^ ((mp_limb_t) random () << 31) ^ ((mp_limb_t) random () << 62);
}
#define __URANDOM
#endif

