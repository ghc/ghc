/* mp.h -- Definitions for Berkeley compatible multiple precision functions.

Copyright (C) 1991, 1993, 1994, 1995, 1996, 2000 Free Software Foundation,
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

#ifndef __MP_H__

#ifndef __GNU_MP__		/* to allow inclusion of both gmp.h and mp.h */
#define __GNU_MP__ 3
#define __need_size_t
#include <stddef.h>
#undef __need_size_t

#if defined (__STDC__) || defined (__cplusplus)
#define __gmp_const const
#else
#define __gmp_const
#endif

#if defined (__GNUC__)
#define __gmp_inline __inline__
#else
#define __gmp_inline
#endif

#ifndef _EXTERN_INLINE
#ifdef __GNUC__
#define _EXTERN_INLINE extern __inline__
#else
#define _EXTERN_INLINE static
#endif
#endif

#ifdef _SHORT_LIMB
typedef unsigned int		mp_limb_t;
typedef int			mp_limb_signed_t;
#else
#ifdef _LONG_LONG_LIMB
typedef unsigned long long int	mp_limb_t;
typedef long long int		mp_limb_signed_t;
#else
typedef unsigned long int	mp_limb_t;
typedef long int		mp_limb_signed_t;
#endif
#endif

typedef mp_limb_t *		mp_ptr;
typedef __gmp_const mp_limb_t *	mp_srcptr;
typedef int			mp_size_t;
typedef long int		mp_exp_t;

typedef struct
{
  int _mp_alloc;		/* Number of *limbs* allocated and pointed
				   to by the D field.  */
  int _mp_size;			/* abs(SIZE) is the number of limbs
				   the last field points to.  If SIZE
				   is negative this is a negative
				   number.  */
  mp_limb_t *_mp_d;		/* Pointer to the limbs.  */
} __mpz_struct;
#endif /* __GNU_MP__ */

/* User-visible types.  */
typedef __mpz_struct MINT;


#ifndef _PROTO
#if (__STDC__-0) || defined (__cplusplus)
#define _PROTO(x) x
#else
#define _PROTO(x) ()
#endif
#endif

#if defined (__cplusplus)
extern "C" {
#endif

void mp_set_memory_functions _PROTO ((void *(*) (size_t),
                                      void *(*) (void *, size_t, size_t),
                                      void (*) (void *, size_t)));
MINT *itom _PROTO ((signed short int));
MINT *xtom _PROTO ((const char *));
void move _PROTO ((const MINT *, MINT *));
void madd _PROTO ((const MINT *, const MINT *, MINT *));
void msub _PROTO ((const MINT *, const MINT *, MINT *));
void mult _PROTO ((const MINT *, const MINT *, MINT *));
void mdiv _PROTO ((const MINT *, const MINT *, MINT *, MINT *));
void sdiv _PROTO ((const MINT *, signed short int, MINT *, signed short int *));
void msqrt _PROTO ((const MINT *, MINT *, MINT *));
void pow _PROTO ((const MINT *, const MINT *, const MINT *, MINT *));
void rpow _PROTO ((const MINT *, signed short int, MINT *));
void gcd _PROTO ((const MINT *, const MINT *, MINT *));
int mcmp _PROTO ((const MINT *, const MINT *));
void min _PROTO ((MINT *));
void mout _PROTO ((const MINT *));
char *mtox _PROTO ((const MINT *));
void mfree _PROTO ((MINT *));

#if defined (__cplusplus)
}
#endif

#define __MP_H__
#endif /* __MP_H__ */
