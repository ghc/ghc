/* gmp.h -- Definitions for GNU multiple precision functions.

Copyright (C) 1991, 1993, 1994, 1995, 1996, 1997, 1999, 2000 Free Software
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

#ifndef __GMP_H__

#ifndef __GNU_MP__		/* to allow inclusion of both gmp.h and mp.h */
#define __GNU_MP__ 2
#define __need_size_t
#include <stddef.h>
#undef __need_size_t

#ifndef STG_H
/* Get DLL_IMPORT */
#include "../../includes/ghcconfig.h"
#include "../../includes/StgDLL.h"
#endif

#if defined (__mips) && defined (_ABIN32)
/* Force the use of 64-bit limbs for all 64-bit MIPS CPUs if ABI permits.  */
#define _LONG_LONG_LIMB
#endif

#if (__STDC__-0) || defined (__cplusplus)
#define __gmp_const const
#define __gmp_signed signed
#else
#define __gmp_const
#define __gmp_signed
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
#if defined (_CRAY) && ! defined (_CRAYMPP)
/* plain `int' is much faster (48 bits) */
typedef int			mp_size_t;
typedef int			mp_exp_t;
#else
typedef long int		mp_size_t;
typedef long int		mp_exp_t;
#endif

typedef struct
{
  int _mp_alloc;		/* Number of *limbs* allocated and pointed
				   to by the _mp_d field.  */
  int _mp_size;			/* abs(_mp_size) is the number of limbs the
				   last field points to.  If _mp_size is
				   negative this is a negative number.  */
  mp_limb_t *_mp_d;		/* Pointer to the limbs.  */
} __mpz_struct;
#endif /* __GNU_MP__ */

typedef __mpz_struct MP_INT;
typedef __mpz_struct mpz_t[1];

typedef struct
{
  __mpz_struct _mp_num;
  __mpz_struct _mp_den;
} __mpq_struct;

typedef __mpq_struct MP_RAT;
typedef __mpq_struct mpq_t[1];

typedef struct
{
  int _mp_prec;			/* Max precision, in number of `mp_limb_t's.
				   Set by mpf_init and modified by
				   mpf_set_prec.  The area pointed to by the
				   _mp_d field contains `prec' + 1 limbs.  */
  int _mp_size;			/* abs(_mp_size) is the number of limbs the
				   last field points to.  If _mp_size is
				   negative this is a negative number.  */
  mp_exp_t _mp_exp;		/* Exponent, in the base of `mp_limb_t'.  */
  mp_limb_t *_mp_d;		/* Pointer to the limbs.  */
} __mpf_struct;

/* typedef __mpf_struct MP_FLOAT; */
typedef __mpf_struct mpf_t[1];

/* Available random number generation algorithms.  */
typedef enum
{
  GMP_RAND_ALG_DEFAULT = 0,
  GMP_RAND_ALG_LC = GMP_RAND_ALG_DEFAULT /* Linear congruential.  */
} gmp_randalg_t;

/* Linear congruential data struct.  */
typedef struct {
  mpz_t a;			/* Multiplier. */
  unsigned long int c;		/* Adder. */
  mpz_t m;			/* Modulus (valid only if m2exp == 0).  */
  unsigned long int m2exp;	/* If != 0, modulus is 2 ^ m2exp.  */
} __gmp_randata_lc;

/* Random state struct.  */
typedef struct
{
  mpz_t seed;			/* Current seed.  */
  gmp_randalg_t alg;		/* Algorithm used.  */
  union {			/* Algorithm specific data.  */
    __gmp_randata_lc *lc;	/* Linear congruential.  */
  } algdata;
} __gmp_randstate_struct;
typedef __gmp_randstate_struct gmp_randstate_t[1];

/* Types for function declarations in gmp files.  */
/* ??? Should not pollute user name space with these ??? */
typedef __gmp_const __mpz_struct *mpz_srcptr;
typedef __mpz_struct *mpz_ptr;
typedef __gmp_const __mpf_struct *mpf_srcptr;
typedef __mpf_struct *mpf_ptr;
typedef __gmp_const __mpq_struct *mpq_srcptr;
typedef __mpq_struct *mpq_ptr;

#ifndef _PROTO
#if (__STDC__-0) || defined (__cplusplus)
#define _PROTO(x) x
#else
#define _PROTO(x) ()
#endif
#endif

#ifndef __MPN
/* Really use `defined (__STDC__)' here; we want it to be true for Sun C */
#if defined (__STDC__) || defined (__cplusplus)
#define __MPN(x) __gmpn_##x
#else
#define __MPN(x) __gmpn_/**/x
#endif
#endif

#if defined (FILE) || defined (H_STDIO) || defined (_H_STDIO) \
 || defined (_STDIO_H) || defined (_STDIO_H_) || defined (__STDIO_H__) \
 || defined (_STDIO_INCLUDED) || defined (__dj_include_stdio_h_)
#define _GMP_H_HAVE_FILE 1
#endif

#if defined (__cplusplus)
extern "C" {
#endif

#define mp_set_memory_functions __gmp_set_memory_functions
DLL_IMPORT void mp_set_memory_functions _PROTO ((void *(*) (size_t),
				      void *(*) (void *, size_t, size_t),
				      void (*) (void *, size_t)));

#define mp_bits_per_limb __gmp_bits_per_limb
DLL_IMPORT extern __gmp_const int mp_bits_per_limb;

#if defined (__cplusplus)
}
#endif


/**************** Random number routines.  ****************/

#define _gmp_rand __gmp_rand
#define gmp_randinit __gmp_randinit
#define gmp_randinit_lc __gmp_randinit_lc
#define gmp_randinit_lc_2exp __gmp_randinit_lc_2exp
#define gmp_randseed __gmp_randseed
#define gmp_randseed_ui __gmp_randseed_ui
#define gmp_randclear __gmp_randclear

#if defined (__cplusplus)
extern "C" {
#endif

DLL_IMPORT void _gmp_rand _PROTO ((mp_ptr, gmp_randstate_t, unsigned long int));
DLL_IMPORT void gmp_randinit _PROTO ((gmp_randstate_t, gmp_randalg_t, ...));
DLL_IMPORT void gmp_randinit_lc _PROTO ((gmp_randstate_t, mpz_t, unsigned long int,
			      mpz_t));
DLL_IMPORT void gmp_randinit_lc_2exp _PROTO ((gmp_randstate_t, mpz_t, unsigned long int,
				   unsigned long int));
DLL_IMPORT void gmp_randseed _PROTO ((gmp_randstate_t, mpz_t));
DLL_IMPORT void gmp_randseed_ui _PROTO ((gmp_randstate_t, unsigned long int));
DLL_IMPORT void gmp_randclear _PROTO ((gmp_randstate_t));

#if defined (__cplusplus)
}
#endif

/**************** Integer (i.e. Z) routines.  ****************/

#define _mpz_realloc __gmpz_realloc
#define mpz_realloc __gmpz_realloc
#define mpz_abs __gmpz_abs
#define mpz_add __gmpz_add
#define mpz_add_ui __gmpz_add_ui
#define mpz_addmul_ui __gmpz_addmul_ui
#define mpz_and __gmpz_and
#define mpz_array_init __gmpz_array_init
#define mpz_bin_ui __gmpz_bin_ui
#define mpz_bin_uiui __gmpz_bin_uiui
#define mpz_cdiv_q __gmpz_cdiv_q
#define mpz_cdiv_q_ui __gmpz_cdiv_q_ui
#define mpz_cdiv_qr __gmpz_cdiv_qr
#define mpz_cdiv_qr_ui __gmpz_cdiv_qr_ui
#define mpz_cdiv_r __gmpz_cdiv_r
#define mpz_cdiv_r_ui __gmpz_cdiv_r_ui
#define mpz_cdiv_ui __gmpz_cdiv_ui
#define mpz_clear __gmpz_clear
#define mpz_clrbit __gmpz_clrbit
#define mpz_cmp __gmpz_cmp
#define _mpz_cmp_si __gmpz_cmp_si
#define _mpz_cmp_ui __gmpz_cmp_ui
#define mpz_cmpabs __gmpz_cmpabs
#define mpz_cmpabs_ui __gmpz_cmpabs_ui
#define mpz_com __gmpz_com
#define mpz_divexact __gmpz_divexact
#define mpz_dump __gmpz_dump
#define mpz_fac_ui __gmpz_fac_ui
#define mpz_fdiv_q __gmpz_fdiv_q
#define mpz_fdiv_q_2exp __gmpz_fdiv_q_2exp
#define mpz_fdiv_q_ui __gmpz_fdiv_q_ui
#define mpz_fdiv_qr __gmpz_fdiv_qr
#define mpz_fdiv_qr_ui __gmpz_fdiv_qr_ui
#define mpz_fdiv_r __gmpz_fdiv_r
#define mpz_fdiv_r_2exp __gmpz_fdiv_r_2exp
#define mpz_fdiv_r_ui __gmpz_fdiv_r_ui
#define mpz_fdiv_ui __gmpz_fdiv_ui
#define mpz_fib_ui __gmpz_fib_ui
#define mpz_fits_sint_p __gmpz_fits_sint_p
#define mpz_fits_slong_p __gmpz_fits_slong_p
#define mpz_fits_sshort_p __gmpz_fits_sshort_p
#define mpz_fits_uint_p __gmpz_fits_uint_p
#define mpz_fits_ulong_p __gmpz_fits_ulong_p
#define mpz_fits_ushort_p __gmpz_fits_ushort_p
#define mpz_gcd __gmpz_gcd
#define mpz_gcd_ui __gmpz_gcd_ui
#define mpz_gcdext __gmpz_gcdext
#define mpz_get_d __gmpz_get_d
#define mpz_get_si __gmpz_get_si
#define mpz_get_str __gmpz_get_str
#define mpz_get_ui __gmpz_get_ui
#define mpz_getlimbn __gmpz_getlimbn
#define mpz_hamdist __gmpz_hamdist
#define mpz_init __gmpz_init
#define mpz_inp_binary __gmpz_inp_binary
#define mpz_inp_raw __gmpz_inp_raw
#define mpz_inp_str __gmpz_inp_str
#define mpz_init_set __gmpz_init_set
#define mpz_init_set_d __gmpz_init_set_d
#define mpz_init_set_si __gmpz_init_set_si
#define mpz_init_set_str __gmpz_init_set_str
#define mpz_init_set_ui __gmpz_init_set_ui
#define mpz_invert __gmpz_invert
#define mpz_ior __gmpz_ior
#define mpz_jacobi __gmpz_jacobi
#define mpz_lcm __gmpz_lcm
#define mpz_legendre __gmpz_legendre
#define mpz_mod __gmpz_mod
#define mpz_mul __gmpz_mul
#define mpz_mul_2exp __gmpz_mul_2exp
#define mpz_neg __gmpz_neg
#define mpz_nextprime __gmpz_nextprime
#define mpz_out_binary __gmpz_out_binary
#define mpz_out_raw __gmpz_out_raw
#define mpz_out_str __gmpz_out_str
#define mpz_perfect_power_p __gmpz_perfect_power_p
#define mpz_perfect_square_p __gmpz_perfect_square_p
#define mpz_popcount __gmpz_popcount
#define mpz_pow_ui __gmpz_pow_ui
#define mpz_powm __gmpz_powm
#define mpz_powm_ui __gmpz_powm_ui
#define mpz_probab_prime_p __gmpz_probab_prime_p
#define mpz_random __gmpz_random
#define mpz_random2 __gmpz_random2
#define mpz_remove __gmpz_remove
#define mpz_root __gmpz_root
#define mpz_rrandomb __gmpz_rrandomb
#define mpz_scan0 __gmpz_scan0
#define mpz_scan1 __gmpz_scan1
#define mpz_set __gmpz_set
#define mpz_set_d __gmpz_set_d
#define mpz_set_f __gmpz_set_f
#define mpz_set_q __gmpz_set_q
#define mpz_set_si __gmpz_set_si
#define mpz_set_str __gmpz_set_str
#define mpz_set_ui __gmpz_set_ui
#define mpz_setbit __gmpz_setbit
#define mpz_size __gmpz_size
#define mpz_sizeinbase __gmpz_sizeinbase
#define mpz_sqrt __gmpz_sqrt
#define mpz_sqrtrem __gmpz_sqrtrem
#define mpz_sub __gmpz_sub
#define mpz_sub_ui __gmpz_sub_ui
#define mpz_swap __gmpz_swap
#define mpz_tdiv_ui __gmpz_tdiv_ui
#define mpz_tdiv_q __gmpz_tdiv_q
#define mpz_tdiv_q_2exp __gmpz_tdiv_q_2exp
#define mpz_tdiv_q_ui __gmpz_tdiv_q_ui
#define mpz_tdiv_qr __gmpz_tdiv_qr
#define mpz_tdiv_qr_ui __gmpz_tdiv_qr_ui
#define mpz_tdiv_r __gmpz_tdiv_r
#define mpz_tdiv_r_2exp __gmpz_tdiv_r_2exp
#define mpz_tdiv_r_ui __gmpz_tdiv_r_ui
#define mpz_tstbit __gmpz_tstbit
#define mpz_ui_pow_ui __gmpz_ui_pow_ui
#define mpz_urandomb __gmpz_urandomb
#define mpz_urandomm __gmpz_urandomm
#define mpz_xor __gmpz_xor
#define mpz_eor __gmpz_xor

#if defined (__cplusplus)
extern "C" {
#endif
DLL_IMPORT void *_mpz_realloc _PROTO ((mpz_ptr, mp_size_t));

DLL_IMPORT void mpz_abs _PROTO ((mpz_ptr, mpz_srcptr));
DLL_IMPORT void mpz_add _PROTO ((mpz_ptr, mpz_srcptr, mpz_srcptr));
DLL_IMPORT void mpz_add_ui _PROTO ((mpz_ptr, mpz_srcptr, unsigned long int));
DLL_IMPORT void mpz_addmul_ui _PROTO ((mpz_ptr, mpz_srcptr, unsigned long int));
DLL_IMPORT void mpz_and _PROTO ((mpz_ptr, mpz_srcptr, mpz_srcptr));
DLL_IMPORT void mpz_array_init _PROTO ((mpz_ptr, mp_size_t, mp_size_t));
DLL_IMPORT void mpz_bin_ui _PROTO ((mpz_ptr, mpz_srcptr, unsigned long int));
DLL_IMPORT void mpz_bin_uiui _PROTO ((mpz_ptr, unsigned long int, unsigned long int));
DLL_IMPORT void mpz_cdiv_q _PROTO ((mpz_ptr, mpz_srcptr, mpz_srcptr));
DLL_IMPORT unsigned long int mpz_cdiv_q_ui _PROTO ((mpz_ptr, mpz_srcptr, unsigned long int));
DLL_IMPORT void mpz_cdiv_qr _PROTO ((mpz_ptr, mpz_ptr, mpz_srcptr, mpz_srcptr));
DLL_IMPORT unsigned long int mpz_cdiv_qr_ui _PROTO ((mpz_ptr, mpz_ptr, mpz_srcptr, unsigned long int));
DLL_IMPORT void mpz_cdiv_r _PROTO ((mpz_ptr, mpz_srcptr, mpz_srcptr));
DLL_IMPORT unsigned long int mpz_cdiv_r_ui _PROTO ((mpz_ptr, mpz_srcptr, unsigned long int));
DLL_IMPORT unsigned long int mpz_cdiv_ui _PROTO ((mpz_srcptr, unsigned long int));
DLL_IMPORT void mpz_clear _PROTO ((mpz_ptr));
DLL_IMPORT void mpz_clrbit _PROTO ((mpz_ptr, unsigned long int));
DLL_IMPORT int mpz_cmp _PROTO ((mpz_srcptr, mpz_srcptr));
DLL_IMPORT int _mpz_cmp_si _PROTO ((mpz_srcptr, signed long int));
DLL_IMPORT int _mpz_cmp_ui _PROTO ((mpz_srcptr, unsigned long int));
DLL_IMPORT int mpz_cmpabs _PROTO ((mpz_srcptr, mpz_srcptr));
DLL_IMPORT int mpz_cmpabs_ui _PROTO ((mpz_srcptr, unsigned long int));
DLL_IMPORT void mpz_com _PROTO ((mpz_ptr, mpz_srcptr));
DLL_IMPORT void mpz_divexact _PROTO ((mpz_ptr, mpz_srcptr, mpz_srcptr));
DLL_IMPORT void mpz_dump _PROTO ((mpz_srcptr));
DLL_IMPORT void mpz_fac_ui _PROTO ((mpz_ptr, unsigned long int));
DLL_IMPORT void mpz_fdiv_q _PROTO ((mpz_ptr, mpz_srcptr, mpz_srcptr));
DLL_IMPORT void mpz_fdiv_q_2exp _PROTO ((mpz_ptr, mpz_srcptr, unsigned long int));
DLL_IMPORT unsigned long int mpz_fdiv_q_ui _PROTO ((mpz_ptr, mpz_srcptr, unsigned long int));
DLL_IMPORT void mpz_fdiv_qr _PROTO ((mpz_ptr, mpz_ptr, mpz_srcptr, mpz_srcptr));
DLL_IMPORT unsigned long int mpz_fdiv_qr_ui _PROTO ((mpz_ptr, mpz_ptr, mpz_srcptr, unsigned long int));
DLL_IMPORT void mpz_fdiv_r _PROTO ((mpz_ptr, mpz_srcptr, mpz_srcptr));
DLL_IMPORT void mpz_fdiv_r_2exp _PROTO ((mpz_ptr, mpz_srcptr, unsigned long int));
DLL_IMPORT unsigned long int mpz_fdiv_r_ui _PROTO ((mpz_ptr, mpz_srcptr, unsigned long int));
DLL_IMPORT unsigned long int mpz_fdiv_ui _PROTO ((mpz_srcptr, unsigned long int));
DLL_IMPORT void mpz_fib_ui _PROTO ((mpz_ptr, unsigned long int));
DLL_IMPORT int mpz_fits_sint_p _PROTO ((mpz_srcptr));
DLL_IMPORT int mpz_fits_slong_p _PROTO ((mpz_srcptr));
DLL_IMPORT int mpz_fits_sshort_p _PROTO ((mpz_srcptr));
DLL_IMPORT int mpz_fits_uint_p _PROTO ((mpz_srcptr));
DLL_IMPORT int mpz_fits_ulong_p _PROTO ((mpz_srcptr));
DLL_IMPORT int mpz_fits_ushort_p _PROTO ((mpz_srcptr));
DLL_IMPORT void mpz_gcd _PROTO ((mpz_ptr, mpz_srcptr, mpz_srcptr));
DLL_IMPORT unsigned long int mpz_gcd_ui _PROTO ((mpz_ptr, mpz_srcptr, unsigned long int));
DLL_IMPORT void mpz_gcdext _PROTO ((mpz_ptr, mpz_ptr, mpz_ptr, mpz_srcptr, mpz_srcptr));
DLL_IMPORT double mpz_get_d _PROTO ((mpz_srcptr));
/* signed */ long int mpz_get_si _PROTO ((mpz_srcptr));
DLL_IMPORT char *mpz_get_str _PROTO ((char *, int, mpz_srcptr));
DLL_IMPORT unsigned long int mpz_get_ui _PROTO ((mpz_srcptr));
DLL_IMPORT mp_limb_t mpz_getlimbn _PROTO ((mpz_srcptr, mp_size_t));
DLL_IMPORT unsigned long int mpz_hamdist _PROTO ((mpz_srcptr, mpz_srcptr));
DLL_IMPORT void mpz_init _PROTO ((mpz_ptr));
#ifdef _GMP_H_HAVE_FILE
DLL_IMPORT size_t mpz_inp_binary _PROTO ((mpz_ptr, FILE *));
DLL_IMPORT size_t mpz_inp_raw _PROTO ((mpz_ptr, FILE *));
DLL_IMPORT size_t mpz_inp_str _PROTO ((mpz_ptr, FILE *, int));
#endif
DLL_IMPORT void mpz_init_set _PROTO ((mpz_ptr, mpz_srcptr));
DLL_IMPORT void mpz_init_set_d _PROTO ((mpz_ptr, double));
DLL_IMPORT void mpz_init_set_si _PROTO ((mpz_ptr, signed long int));
DLL_IMPORT int mpz_init_set_str _PROTO ((mpz_ptr, __gmp_const char *, int));
DLL_IMPORT void mpz_init_set_ui _PROTO ((mpz_ptr, unsigned long int));
DLL_IMPORT int mpz_invert _PROTO ((mpz_ptr, mpz_srcptr, mpz_srcptr));
DLL_IMPORT void mpz_ior _PROTO ((mpz_ptr, mpz_srcptr, mpz_srcptr));
DLL_IMPORT int mpz_jacobi _PROTO ((mpz_srcptr, mpz_srcptr));

#define mpz_kronecker_si __gmpz_kronecker_si
DLL_IMPORT int mpz_kronecker_si _PROTO ((mpz_srcptr, long));

#define mpz_kronecker_ui __gmpz_kronecker_ui
DLL_IMPORT int mpz_kronecker_ui _PROTO ((mpz_srcptr, unsigned long));

#define mpz_si_kronecker __gmpz_si_kronecker
DLL_IMPORT int mpz_si_kronecker _PROTO ((long, mpz_srcptr));

#define mpz_ui_kronecker __gmpz_ui_kronecker
DLL_IMPORT int mpz_ui_kronecker _PROTO ((unsigned long, mpz_srcptr));

DLL_IMPORT void mpz_lcm _PROTO ((mpz_ptr, mpz_srcptr, mpz_srcptr));
DLL_IMPORT int mpz_legendre _PROTO ((mpz_srcptr, mpz_srcptr));
DLL_IMPORT void mpz_mod _PROTO ((mpz_ptr, mpz_srcptr, mpz_srcptr));
DLL_IMPORT void mpz_mul _PROTO ((mpz_ptr, mpz_srcptr, mpz_srcptr));
DLL_IMPORT void mpz_mul_2exp _PROTO ((mpz_ptr, mpz_srcptr, unsigned long int));

#define mpz_mul_si __gmpz_mul_si
DLL_IMPORT void mpz_mul_si _PROTO ((mpz_ptr, mpz_srcptr, long int));

#define mpz_mul_ui __gmpz_mul_ui
DLL_IMPORT void mpz_mul_ui _PROTO ((mpz_ptr, mpz_srcptr, unsigned long int));

DLL_IMPORT void mpz_neg _PROTO ((mpz_ptr, mpz_srcptr));
DLL_IMPORT void mpz_nextprime _PROTO ((mpz_ptr, mpz_srcptr));
#ifdef _GMP_H_HAVE_FILE
DLL_IMPORT size_t mpz_out_binary _PROTO ((FILE *, mpz_srcptr));
DLL_IMPORT size_t mpz_out_raw _PROTO ((FILE *, mpz_srcptr));
DLL_IMPORT size_t mpz_out_str _PROTO ((FILE *, int, mpz_srcptr));
#endif
DLL_IMPORT int mpz_perfect_power_p _PROTO ((mpz_srcptr));
DLL_IMPORT int mpz_perfect_square_p _PROTO ((mpz_srcptr));
DLL_IMPORT unsigned long int mpz_popcount _PROTO ((mpz_srcptr));
DLL_IMPORT void mpz_pow_ui _PROTO ((mpz_ptr, mpz_srcptr, unsigned long int));
DLL_IMPORT void mpz_powm _PROTO ((mpz_ptr, mpz_srcptr, mpz_srcptr, mpz_srcptr));
DLL_IMPORT void mpz_powm_ui _PROTO ((mpz_ptr, mpz_srcptr, unsigned long int, mpz_srcptr));
DLL_IMPORT int mpz_probab_prime_p _PROTO ((mpz_srcptr, int));
DLL_IMPORT void mpz_random _PROTO ((mpz_ptr, mp_size_t));
DLL_IMPORT void mpz_random2 _PROTO ((mpz_ptr, mp_size_t));
DLL_IMPORT unsigned long int mpz_remove _PROTO ((mpz_ptr, mpz_srcptr, mpz_srcptr));
DLL_IMPORT int mpz_root _PROTO ((mpz_ptr, mpz_srcptr, unsigned long int));
DLL_IMPORT void mpz_rrandomb _PROTO ((mpz_ptr, gmp_randstate_t, unsigned long int));
DLL_IMPORT unsigned long int mpz_scan0 _PROTO ((mpz_srcptr, unsigned long int));
DLL_IMPORT unsigned long int mpz_scan1 _PROTO ((mpz_srcptr, unsigned long int));
DLL_IMPORT void mpz_set _PROTO ((mpz_ptr, mpz_srcptr));
DLL_IMPORT void mpz_set_d _PROTO ((mpz_ptr, double));
DLL_IMPORT void mpz_set_f _PROTO ((mpz_ptr, mpf_srcptr));
DLL_IMPORT void mpz_set_q _PROTO ((mpz_ptr, mpq_srcptr));
DLL_IMPORT void mpz_set_si _PROTO ((mpz_ptr, signed long int));
DLL_IMPORT int mpz_set_str _PROTO ((mpz_ptr, __gmp_const char *, int));
DLL_IMPORT void mpz_set_ui _PROTO ((mpz_ptr, unsigned long int));
DLL_IMPORT void mpz_setbit _PROTO ((mpz_ptr, unsigned long int));
DLL_IMPORT size_t mpz_size _PROTO ((mpz_srcptr));
DLL_IMPORT size_t mpz_sizeinbase _PROTO ((mpz_srcptr, int));
DLL_IMPORT void mpz_sqrt _PROTO ((mpz_ptr, mpz_srcptr));
DLL_IMPORT void mpz_sqrtrem _PROTO ((mpz_ptr, mpz_ptr, mpz_srcptr));
DLL_IMPORT void mpz_sub _PROTO ((mpz_ptr, mpz_srcptr, mpz_srcptr));
DLL_IMPORT void mpz_sub_ui _PROTO ((mpz_ptr, mpz_srcptr, unsigned long int));
DLL_IMPORT void mpz_swap _PROTO ((mpz_ptr, mpz_ptr));
DLL_IMPORT void mpz_tdiv_q _PROTO ((mpz_ptr, mpz_srcptr, mpz_srcptr));
DLL_IMPORT void mpz_tdiv_q_2exp _PROTO ((mpz_ptr, mpz_srcptr, unsigned long int));
DLL_IMPORT unsigned long int mpz_tdiv_ui _PROTO ((mpz_srcptr, unsigned long int));
DLL_IMPORT unsigned long int mpz_tdiv_q_ui _PROTO ((mpz_ptr, mpz_srcptr, unsigned long int));
DLL_IMPORT void mpz_tdiv_qr _PROTO ((mpz_ptr, mpz_ptr, mpz_srcptr, mpz_srcptr));
DLL_IMPORT unsigned long int mpz_tdiv_qr_ui _PROTO ((mpz_ptr, mpz_ptr, mpz_srcptr, unsigned long int));
DLL_IMPORT void mpz_tdiv_r _PROTO ((mpz_ptr, mpz_srcptr, mpz_srcptr));
DLL_IMPORT void mpz_tdiv_r_2exp _PROTO ((mpz_ptr, mpz_srcptr, unsigned long int));
DLL_IMPORT unsigned long int mpz_tdiv_r_ui _PROTO ((mpz_ptr, mpz_srcptr, unsigned long int));
DLL_IMPORT int mpz_tstbit _PROTO ((mpz_srcptr, unsigned long int));
DLL_IMPORT void mpz_ui_pow_ui _PROTO ((mpz_ptr, unsigned long int, unsigned long int));
DLL_IMPORT void mpz_urandomb _PROTO ((mpz_t, gmp_randstate_t, unsigned long int));
DLL_IMPORT void mpz_urandomm _PROTO ((mpz_t, gmp_randstate_t, mpz_t));
DLL_IMPORT void mpz_xor _PROTO ((mpz_ptr, mpz_srcptr, mpz_srcptr));
#if defined (__cplusplus)
}
#endif

/**************** Rational (i.e. Q) routines.  ****************/

#define mpq_init __gmpq_init
#define mpq_clear __gmpq_clear
#define mpq_set __gmpq_set
#define mpq_set_ui __gmpq_set_ui
#define mpq_set_si __gmpq_set_si
#define mpq_set_z __gmpq_set_z
#define mpq_add __gmpq_add
#define mpq_sub __gmpq_sub
#define mpq_mul __gmpq_mul
#define mpq_div __gmpq_div
#define mpq_neg __gmpq_neg
#define mpq_cmp __gmpq_cmp
#define _mpq_cmp_ui __gmpq_cmp_ui
#define mpq_equal __gmpq_equal
#define mpq_inv __gmpq_inv
#define mpq_set_num __gmpq_set_num
#define mpq_set_den __gmpq_set_den
#define mpq_get_num __gmpq_get_num
#define mpq_get_den __gmpq_get_den
#define mpq_get_d __gmpq_get_d
#define mpq_set_d __gmpq_set_d
#define mpq_canonicalize __gmpq_canonicalize

#if defined (__cplusplus)
extern "C" {
#endif
DLL_IMPORT void mpq_init _PROTO ((mpq_ptr));
DLL_IMPORT void mpq_clear _PROTO ((mpq_ptr));
DLL_IMPORT void mpq_set _PROTO ((mpq_ptr, mpq_srcptr));
DLL_IMPORT void mpq_set_ui _PROTO ((mpq_ptr, unsigned long int, unsigned long int));
DLL_IMPORT void mpq_set_si _PROTO ((mpq_ptr, signed long int, unsigned long int));
DLL_IMPORT void mpq_set_z _PROTO ((mpq_ptr, mpz_srcptr));
DLL_IMPORT void mpq_add _PROTO ((mpq_ptr, mpq_srcptr, mpq_srcptr));
DLL_IMPORT void mpq_sub _PROTO ((mpq_ptr, mpq_srcptr, mpq_srcptr));
DLL_IMPORT void mpq_mul _PROTO ((mpq_ptr, mpq_srcptr, mpq_srcptr));
DLL_IMPORT void mpq_div _PROTO ((mpq_ptr, mpq_srcptr, mpq_srcptr));
DLL_IMPORT void mpq_neg _PROTO ((mpq_ptr, mpq_srcptr));
DLL_IMPORT int mpq_cmp _PROTO ((mpq_srcptr, mpq_srcptr));
DLL_IMPORT int _mpq_cmp_ui _PROTO ((mpq_srcptr, unsigned long int, unsigned long int));
DLL_IMPORT int mpq_equal _PROTO ((mpq_srcptr, mpq_srcptr));
DLL_IMPORT void mpq_inv _PROTO ((mpq_ptr, mpq_srcptr));
DLL_IMPORT void mpq_set_num _PROTO ((mpq_ptr, mpz_srcptr));
DLL_IMPORT void mpq_set_den _PROTO ((mpq_ptr, mpz_srcptr));
DLL_IMPORT void mpq_get_num _PROTO ((mpz_ptr, mpq_srcptr));
DLL_IMPORT void mpq_get_den _PROTO ((mpz_ptr, mpq_srcptr));
DLL_IMPORT double mpq_get_d _PROTO ((mpq_srcptr));
DLL_IMPORT void mpq_set_d _PROTO ((mpq_ptr, double));
DLL_IMPORT void mpq_canonicalize _PROTO ((mpq_ptr));

#define mpq_swap __gmpq_swap
DLL_IMPORT void mpq_swap _PROTO ((mpq_ptr, mpq_ptr));

#ifdef _GMP_H_HAVE_FILE
#define mpq_out_str __gmpq_out_str
DLL_IMPORT size_t mpq_out_str _PROTO ((FILE *, int, mpq_srcptr));
#endif

#if defined (__cplusplus)
}
#endif

/**************** Float (i.e. F) routines.  ****************/

#define mpf_abs __gmpf_abs
#define mpf_add __gmpf_add
#define mpf_add_ui __gmpf_add_ui
#define mpf_ceil __gmpf_ceil
#define mpf_clear __gmpf_clear
#define mpf_cmp __gmpf_cmp
#define mpf_cmp_si __gmpf_cmp_si
#define mpf_cmp_ui __gmpf_cmp_ui
#define mpf_div __gmpf_div
#define mpf_div_2exp __gmpf_div_2exp
#define mpf_div_ui __gmpf_div_ui
#define mpf_dump __gmpf_dump
#define mpf_floor __gmpf_floor
#define mpf_eq __gmpf_eq
#define mpf_get_d __gmpf_get_d
#define mpf_get_prec __gmpf_get_prec
#define mpf_get_str __gmpf_get_str
#define mpf_init __gmpf_init
#define mpf_init2 __gmpf_init2
#define mpf_inp_str __gmpf_inp_str
#define mpf_init_set __gmpf_init_set
#define mpf_init_set_d __gmpf_init_set_d
#define mpf_init_set_si __gmpf_init_set_si
#define mpf_init_set_str __gmpf_init_set_str
#define mpf_init_set_ui __gmpf_init_set_ui
#define mpf_mul __gmpf_mul
#define mpf_mul_2exp __gmpf_mul_2exp
#define mpf_mul_ui __gmpf_mul_ui
#define mpf_neg __gmpf_neg
#define mpf_out_str __gmpf_out_str
#define mpf_pow_ui __gmpf_pow_ui
#define mpf_random2 __gmpf_random2
#define mpf_reldiff __gmpf_reldiff
#define mpf_set __gmpf_set
#define mpf_set_d __gmpf_set_d
#define mpf_set_default_prec __gmpf_set_default_prec
#define mpf_set_prec __gmpf_set_prec
#define mpf_set_prec_raw __gmpf_set_prec_raw
#define mpf_set_q __gmpf_set_q
#define mpf_set_si __gmpf_set_si
#define mpf_set_str __gmpf_set_str
#define mpf_set_ui __gmpf_set_ui
#define mpf_set_z __gmpf_set_z
#define mpf_size __gmpf_size
#define mpf_sqrt __gmpf_sqrt
#define mpf_sqrt_ui __gmpf_sqrt_ui
#define mpf_sub __gmpf_sub
#define mpf_sub_ui __gmpf_sub_ui
#define mpf_trunc __gmpf_trunc
#define mpf_ui_div __gmpf_ui_div
#define mpf_ui_sub __gmpf_ui_sub
#define mpf_urandomb __gmpf_urandomb

#if defined (__cplusplus)
extern "C" {
#endif
DLL_IMPORT void mpf_abs _PROTO ((mpf_ptr, mpf_srcptr));
DLL_IMPORT void mpf_add _PROTO ((mpf_ptr, mpf_srcptr, mpf_srcptr));
DLL_IMPORT void mpf_add_ui _PROTO ((mpf_ptr, mpf_srcptr, unsigned long int));
DLL_IMPORT void mpf_ceil _PROTO ((mpf_ptr, mpf_srcptr));
DLL_IMPORT void mpf_clear _PROTO ((mpf_ptr));
DLL_IMPORT int mpf_cmp _PROTO ((mpf_srcptr, mpf_srcptr));
DLL_IMPORT int mpf_cmp_si _PROTO ((mpf_srcptr, signed long int));
DLL_IMPORT int mpf_cmp_ui _PROTO ((mpf_srcptr, unsigned long int));
DLL_IMPORT void mpf_div _PROTO ((mpf_ptr, mpf_srcptr, mpf_srcptr));
DLL_IMPORT void mpf_div_2exp _PROTO ((mpf_ptr, mpf_srcptr, unsigned long int));
DLL_IMPORT void mpf_div_ui _PROTO ((mpf_ptr, mpf_srcptr, unsigned long int));
DLL_IMPORT void mpf_dump _PROTO ((mpf_srcptr));
DLL_IMPORT int mpf_eq _PROTO ((mpf_srcptr, mpf_srcptr, unsigned long int));
DLL_IMPORT void mpf_floor _PROTO ((mpf_ptr, mpf_srcptr));
DLL_IMPORT double mpf_get_d _PROTO ((mpf_srcptr));
DLL_IMPORT unsigned long int mpf_get_prec _PROTO ((mpf_srcptr));
char *mpf_get_str _PROTO ((char *, mp_exp_t *, int, size_t, mpf_srcptr));
DLL_IMPORT void mpf_init _PROTO ((mpf_ptr));
DLL_IMPORT void mpf_init2 _PROTO ((mpf_ptr, unsigned long int));
#ifdef _GMP_H_HAVE_FILE
DLL_IMPORT size_t mpf_inp_str _PROTO ((mpf_ptr, FILE *, int));
#endif
DLL_IMPORT void mpf_init_set _PROTO ((mpf_ptr, mpf_srcptr));
DLL_IMPORT void mpf_init_set_d _PROTO ((mpf_ptr, double));
DLL_IMPORT void mpf_init_set_si _PROTO ((mpf_ptr, signed long int));
DLL_IMPORT int mpf_init_set_str _PROTO ((mpf_ptr, __gmp_const char *, int));
DLL_IMPORT void mpf_init_set_ui _PROTO ((mpf_ptr, unsigned long int));
DLL_IMPORT void mpf_mul _PROTO ((mpf_ptr, mpf_srcptr, mpf_srcptr));
DLL_IMPORT void mpf_mul_2exp _PROTO ((mpf_ptr, mpf_srcptr, unsigned long int));
DLL_IMPORT void mpf_mul_ui _PROTO ((mpf_ptr, mpf_srcptr, unsigned long int));
DLL_IMPORT void mpf_neg _PROTO ((mpf_ptr, mpf_srcptr));
#ifdef _GMP_H_HAVE_FILE
DLL_IMPORT size_t mpf_out_str _PROTO ((FILE *, int, size_t, mpf_srcptr));
#endif
DLL_IMPORT void mpf_pow_ui _PROTO ((mpf_ptr, mpf_srcptr, unsigned long int));
DLL_IMPORT void mpf_random2 _PROTO ((mpf_ptr, mp_size_t, mp_exp_t));
DLL_IMPORT void mpf_reldiff _PROTO ((mpf_ptr, mpf_srcptr, mpf_srcptr));
DLL_IMPORT void mpf_set _PROTO ((mpf_ptr, mpf_srcptr));
DLL_IMPORT void mpf_set_d _PROTO ((mpf_ptr, double));
DLL_IMPORT void mpf_set_default_prec _PROTO ((unsigned long int));
DLL_IMPORT void mpf_set_prec _PROTO ((mpf_ptr, unsigned long int));
DLL_IMPORT void mpf_set_prec_raw _PROTO ((mpf_ptr, unsigned long int));
DLL_IMPORT void mpf_set_q _PROTO ((mpf_ptr, mpq_srcptr));
DLL_IMPORT void mpf_set_si _PROTO ((mpf_ptr, signed long int));
DLL_IMPORT int mpf_set_str _PROTO ((mpf_ptr, __gmp_const char *, int));
DLL_IMPORT void mpf_set_ui _PROTO ((mpf_ptr, unsigned long int));
DLL_IMPORT void mpf_set_z _PROTO ((mpf_ptr, mpz_srcptr));
DLL_IMPORT size_t mpf_size _PROTO ((mpf_srcptr));
DLL_IMPORT void mpf_sqrt _PROTO ((mpf_ptr, mpf_srcptr));
DLL_IMPORT void mpf_sqrt_ui _PROTO ((mpf_ptr, unsigned long int));
DLL_IMPORT void mpf_sub _PROTO ((mpf_ptr, mpf_srcptr, mpf_srcptr));
DLL_IMPORT void mpf_sub_ui _PROTO ((mpf_ptr, mpf_srcptr, unsigned long int));
DLL_IMPORT void mpf_trunc _PROTO ((mpf_ptr, mpf_srcptr));
DLL_IMPORT void mpf_ui_div _PROTO ((mpf_ptr, unsigned long int, mpf_srcptr));
DLL_IMPORT void mpf_ui_sub _PROTO ((mpf_ptr, unsigned long int, mpf_srcptr));
DLL_IMPORT void mpf_urandomb _PROTO ((mpf_t, gmp_randstate_t, unsigned long int));

#define mpf_swap __gmpf_swap
DLL_IMPORT void mpf_swap _PROTO ((mpf_ptr, mpf_ptr));

#if defined (__cplusplus)
}
#endif
/************ Low level positive-integer (i.e. N) routines.  ************/

/* This is ugly, but we need to make user calls reach the prefixed function. */
#define mpn_add			__MPN(add)
#define mpn_add_1		__MPN(add_1)
#define mpn_add_n		__MPN(add_n)
#define mpn_add_nc		__MPN(add_nc)
#define mpn_addmul_1		__MPN(addmul_1)
#define mpn_addsub_n		__MPN(addsub_n)
#define mpn_addsub_nc		__MPN(addsub_nc)
/* #define mpn_and_n		__MPN(and_n) */
/* #define mpn_andn_n		__MPN(andn_n) */
#define mpn_bdivmod		__MPN(bdivmod)
#define mpn_cmp			__MPN(cmp)
/* #define mpn_com_n		__MPN(com_n) */
#define mpn_copyd		__MPN(copyd)
#define mpn_copyi		__MPN(copyi)
#define mpn_divrem		__MPN(divrem)
#define mpn_divrem_1		__MPN(divrem_1)
#define mpn_divrem_2		__MPN(divrem_2)
#define mpn_dump		__MPN(dump)
#define mpn_gcd			__MPN(gcd)
#define mpn_gcd_1		__MPN(gcd_1)
#define mpn_gcdext		__MPN(gcdext)
#define mpn_get_str		__MPN(get_str)
#define mpn_hamdist		__MPN(hamdist)
#define mpn_invert_limb 	__MPN(invert_limb)
/* #define mpn_ior_n		__MPN(ior_n) */
/* #define mpn_iorn_n		__MPN(iorn_n) */
/* #define mpn_kara_mul_n	__MPN(kara_mul_n)  internal */
/* #define mpn_kara_sqr_n	__MPN(kara_sqr_n)  internal */
#define mpn_lshift		__MPN(lshift)
#define mpn_lshiftc		__MPN(lshiftc)
#define mpn_mod_1		__MPN(mod_1)
#define mpn_mul			__MPN(mul)
#define mpn_mul_1		__MPN(mul_1)
#define mpn_mul_basecase	__MPN(mul_basecase)
#define mpn_mul_n		__MPN(mul_n)
#define mpn_perfect_square_p	__MPN(perfect_square_p)
#define mpn_popcount		__MPN(popcount)
#define mpn_preinv_mod_1	__MPN(preinv_mod_1)
/* #define mpn_nand_n		__MPN(nand_n) */
/* #define mpn_nior_n		__MPN(nior_n) */
#define mpn_random		__MPN(random)
#define mpn_random2		__MPN(random2)
#define mpn_rshift		__MPN(rshift)
#define mpn_rshiftc		__MPN(rshiftc)
#define mpn_scan0		__MPN(scan0)
#define mpn_scan1		__MPN(scan1)
#define mpn_set_str		__MPN(set_str)
#define mpn_sqr_basecase	__MPN(sqr_basecase)
#define mpn_sqr_n		__MPN(sqr_n)
#define mpn_sqrtrem		__MPN(sqrtrem)
#define mpn_sub			__MPN(sub)
#define mpn_sub_1		__MPN(sub_1)
#define mpn_sub_n		__MPN(sub_n)
#define mpn_sub_nc		__MPN(sub_nc)
#define mpn_submul_1		__MPN(submul_1)
/* #define mpn_toom3_mul_n		__MPN(toom3_mul_n)  internal */
/* #define mpn_toom3_sqr_n		__MPN(toom3_sqr_n)  internal */
/* #define mpn_xnor_n		__MPN(xnor_n) */
/* #define mpn_xor_n		__MPN(xor_n) */

#if defined (__cplusplus)
extern "C" {
#endif

DLL_IMPORT mp_limb_t mpn_add _PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_srcptr,mp_size_t));
DLL_IMPORT mp_limb_t mpn_add_1 _PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_limb_t));
DLL_IMPORT mp_limb_t mpn_add_n _PROTO ((mp_ptr, mp_srcptr, mp_srcptr, mp_size_t));
DLL_IMPORT mp_limb_t mpn_add_nc _PROTO ((mp_ptr, mp_srcptr, mp_srcptr, mp_size_t, mp_limb_t));

DLL_IMPORT mp_limb_t mpn_addmul_1 _PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_limb_t));

#define mpn_addmul_1c  __MPN(addmul_1c)
DLL_IMPORT mp_limb_t mpn_addmul_1c _PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_limb_t, mp_limb_t));

DLL_IMPORT mp_limb_t mpn_addsub_n _PROTO ((mp_ptr, mp_ptr, mp_srcptr, mp_srcptr, mp_size_t));
DLL_IMPORT mp_limb_t mpn_bdivmod _PROTO ((mp_ptr, mp_ptr, mp_size_t, mp_srcptr, mp_size_t, unsigned long int));
DLL_IMPORT int mpn_cmp _PROTO ((mp_srcptr, mp_srcptr, mp_size_t));

#define mpn_divexact_by3(dst, src, size)  mpn_divexact_by3c (dst, src, size, 0)

#define mpn_divexact_by3c  __MPN(divexact_by3c)
DLL_IMPORT mp_limb_t mpn_divexact_by3c _PROTO ((mp_ptr dst, mp_srcptr src,
                                     mp_size_t size, mp_limb_t carry));

#define mpn_divmod_1(qp,np,nsize,dlimb) mpn_divrem_1 (qp,0,np,nsize,dlimb)

DLL_IMPORT mp_limb_t mpn_divrem _PROTO((mp_ptr, mp_size_t, mp_ptr, mp_size_t, mp_srcptr, mp_size_t));

DLL_IMPORT mp_limb_t mpn_divrem_1 _PROTO ((mp_ptr, mp_size_t, mp_srcptr, mp_size_t, mp_limb_t));

#define mpn_divrem_1c  __MPN(divrem_1c)
DLL_IMPORT mp_limb_t mpn_divrem_1c _PROTO ((mp_ptr, mp_size_t, mp_srcptr, mp_size_t,
                                 mp_limb_t, mp_limb_t));

DLL_IMPORT mp_limb_t mpn_divrem_2 _PROTO ((mp_ptr, mp_size_t, mp_ptr, mp_size_t, mp_srcptr));
DLL_IMPORT void mpn_dump _PROTO ((mp_srcptr, mp_size_t));
mp_size_t mpn_gcd _PROTO ((mp_ptr, mp_ptr, mp_size_t, mp_ptr, mp_size_t));
DLL_IMPORT mp_limb_t mpn_gcd_1 _PROTO ((mp_srcptr, mp_size_t, mp_limb_t));
mp_size_t mpn_gcdext _PROTO ((mp_ptr, mp_ptr, mp_size_t *, mp_ptr, mp_size_t, mp_ptr, mp_size_t));
DLL_IMPORT size_t mpn_get_str _PROTO ((unsigned char *, int, mp_ptr, mp_size_t));
DLL_IMPORT unsigned long int mpn_hamdist _PROTO ((mp_srcptr, mp_srcptr, mp_size_t));

#define mpn_jacobi_base __MPN(jacobi_base)
DLL_IMPORT int mpn_jacobi_base _PROTO ((mp_limb_t a, mp_limb_t b, int result_bit1));

DLL_IMPORT mp_limb_t mpn_lshift _PROTO ((mp_ptr, mp_srcptr, mp_size_t, unsigned int));
DLL_IMPORT mp_limb_t mpn_mod_1 _PROTO ((mp_srcptr, mp_size_t, mp_limb_t));

#define mpn_mod_1c  __MPN(mod_1c)
DLL_IMPORT mp_limb_t mpn_mod_1c _PROTO ((mp_srcptr, mp_size_t, mp_limb_t, mp_limb_t));

#define mpn_mod_1_rshift __MPN(mod_1_rshift)
DLL_IMPORT mp_limb_t mpn_mod_1_rshift _PROTO ((mp_srcptr, mp_size_t, unsigned,mp_limb_t));

DLL_IMPORT mp_limb_t mpn_mul _PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_srcptr, mp_size_t));
DLL_IMPORT mp_limb_t mpn_mul_1 _PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_limb_t));

#define mpn_mul_1c  __MPN(mul_1c)
DLL_IMPORT mp_limb_t mpn_mul_1c _PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_limb_t, mp_limb_t));

DLL_IMPORT void mpn_mul_basecase _PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_srcptr, mp_size_t));
DLL_IMPORT void mpn_mul_n _PROTO ((mp_ptr, mp_srcptr, mp_srcptr, mp_size_t));
DLL_IMPORT int mpn_perfect_square_p _PROTO ((mp_srcptr, mp_size_t));
DLL_IMPORT unsigned long int mpn_popcount _PROTO ((mp_srcptr, mp_size_t));
DLL_IMPORT mp_limb_t mpn_preinv_mod_1 _PROTO ((mp_srcptr, mp_size_t, mp_limb_t, mp_limb_t));
DLL_IMPORT void mpn_random _PROTO ((mp_ptr, mp_size_t));
DLL_IMPORT void mpn_random2 _PROTO ((mp_ptr, mp_size_t));
DLL_IMPORT mp_limb_t mpn_rshift _PROTO ((mp_ptr, mp_srcptr, mp_size_t, unsigned int));
DLL_IMPORT unsigned long int mpn_scan0 _PROTO ((mp_srcptr, unsigned long int));
DLL_IMPORT unsigned long int mpn_scan1 _PROTO ((mp_srcptr, unsigned long int));
mp_size_t mpn_set_str _PROTO ((mp_ptr, __gmp_const unsigned char *, size_t, int));
DLL_IMPORT void mpn_sqr_n _PROTO ((mp_ptr, mp_srcptr, mp_size_t));
DLL_IMPORT void mpn_sqr_basecase _PROTO ((mp_ptr, mp_srcptr, mp_size_t));
mp_size_t mpn_sqrtrem _PROTO ((mp_ptr, mp_ptr, mp_srcptr, mp_size_t));
DLL_IMPORT mp_limb_t mpn_sub _PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_srcptr,mp_size_t));
DLL_IMPORT mp_limb_t mpn_sub_1 _PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_limb_t));
DLL_IMPORT mp_limb_t mpn_sub_n _PROTO ((mp_ptr, mp_srcptr, mp_srcptr, mp_size_t));
DLL_IMPORT mp_limb_t mpn_sub_nc _PROTO ((mp_ptr, mp_srcptr, mp_srcptr, mp_size_t, mp_limb_t));
DLL_IMPORT mp_limb_t mpn_submul_1 _PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_limb_t));

#define mpn_submul_1c  __MPN(submul_1c)
DLL_IMPORT mp_limb_t mpn_submul_1c _PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_limb_t, mp_limb_t));

#define mpn_tdiv_qr  __MPN(tdiv_qr)
DLL_IMPORT void mpn_tdiv_qr _PROTO ((mp_ptr, mp_ptr, mp_size_t, mp_srcptr, mp_size_t, mp_srcptr, mp_size_t));

#if defined (__cplusplus)
}
#endif

#define mpn_incr_u(p,incr) \
  do { mp_limb_t __x; mp_ptr __p = p;			\
    __x = *__p + incr;					\
    *__p = __x;						\
    if (__x < incr)					\
      while (++(*(++__p)) == 0)				\
        ;						\
  } while (0)

#define mpn_decr_u(p,incr) \
  do { mp_limb_t __x; mp_ptr __p = p;			\
    __x = *__p;						\
    *__p = __x - incr;					\
    if (__x < incr)					\
      while ((*(++__p))-- == 0)				\
        ;						\
  } while (0)

#if defined (__GNUC__) || defined (_FORCE_INLINES)
_EXTERN_INLINE mp_limb_t
#if (__STDC__-0) || defined (__cplusplus)
mpn_add_1 (register mp_ptr res_ptr,
	   register mp_srcptr s1_ptr,
	   register mp_size_t s1_size,
	   register mp_limb_t s2_limb)
#else
mpn_add_1 (res_ptr, s1_ptr, s1_size, s2_limb)
     register mp_ptr res_ptr;
     register mp_srcptr s1_ptr;
     register mp_size_t s1_size;
     register mp_limb_t s2_limb;
#endif
{
  register mp_limb_t x;

  x = *s1_ptr++;
  s2_limb = x + s2_limb;
  *res_ptr++ = s2_limb;
  if (s2_limb < x)
    {
      while (--s1_size != 0)
	{
	  x = *s1_ptr++ + 1;
	  *res_ptr++ = x;
	  if (x != 0)
	    goto fin;
	}

      return 1;
    }

 fin:
  if (res_ptr != s1_ptr)
    {
      mp_size_t i;
      for (i = 0; i < s1_size - 1; i++)
	res_ptr[i] = s1_ptr[i];
    }
  return 0;
}

_EXTERN_INLINE mp_limb_t
#if (__STDC__-0) || defined (__cplusplus)
mpn_add (register mp_ptr res_ptr,
	 register mp_srcptr s1_ptr,
	 register mp_size_t s1_size,
	 register mp_srcptr s2_ptr,
	 register mp_size_t s2_size)
#else
mpn_add (res_ptr, s1_ptr, s1_size, s2_ptr, s2_size)
     register mp_ptr res_ptr;
     register mp_srcptr s1_ptr;
     register mp_size_t s1_size;
     register mp_srcptr s2_ptr;
     register mp_size_t s2_size;
#endif
{
  mp_limb_t cy_limb = 0;

  if (s2_size != 0)
    cy_limb = mpn_add_n (res_ptr, s1_ptr, s2_ptr, s2_size);

  if (s1_size - s2_size != 0)
    cy_limb = mpn_add_1 (res_ptr + s2_size,
			 s1_ptr + s2_size,
			 s1_size - s2_size,
			 cy_limb);
  return cy_limb;
}

_EXTERN_INLINE mp_limb_t
#if (__STDC__-0) || defined (__cplusplus)
mpn_sub_1 (register mp_ptr res_ptr,
	   register mp_srcptr s1_ptr,
	   register mp_size_t s1_size,
	   register mp_limb_t s2_limb)
#else
mpn_sub_1 (res_ptr, s1_ptr, s1_size, s2_limb)
     register mp_ptr res_ptr;
     register mp_srcptr s1_ptr;
     register mp_size_t s1_size;
     register mp_limb_t s2_limb;
#endif
{
  register mp_limb_t x;

  x = *s1_ptr++;
  s2_limb = x - s2_limb;
  *res_ptr++ = s2_limb;
  if (s2_limb > x)
    {
      while (--s1_size != 0)
	{
	  x = *s1_ptr++;
	  *res_ptr++ = x - 1;
	  if (x != 0)
	    goto fin;
	}

      return 1;
    }

 fin:
  if (res_ptr != s1_ptr)
    {
      mp_size_t i;
      for (i = 0; i < s1_size - 1; i++)
	res_ptr[i] = s1_ptr[i];
    }
  return 0;
}

_EXTERN_INLINE mp_limb_t
#if (__STDC__-0) || defined (__cplusplus)
mpn_sub (register mp_ptr res_ptr,
	 register mp_srcptr s1_ptr,
	 register mp_size_t s1_size,
	 register mp_srcptr s2_ptr,
	 register mp_size_t s2_size)
#else
mpn_sub (res_ptr, s1_ptr, s1_size, s2_ptr, s2_size)
     register mp_ptr res_ptr;
     register mp_srcptr s1_ptr;
     register mp_size_t s1_size;
     register mp_srcptr s2_ptr;
     register mp_size_t s2_size;
#endif
{
  mp_limb_t cy_limb = 0;

  if (s2_size != 0)
    cy_limb = mpn_sub_n (res_ptr, s1_ptr, s2_ptr, s2_size);

  if (s1_size - s2_size != 0)
    cy_limb = mpn_sub_1 (res_ptr + s2_size,
			 s1_ptr + s2_size,
			 s1_size - s2_size,
			 cy_limb);
  return cy_limb;
}
#endif /* __GNUC__ */

/* Allow faster testing for negative, zero, and positive.  */
#define mpz_sgn(Z) ((Z)->_mp_size < 0 ? -1 : (Z)->_mp_size > 0)
#define mpf_sgn(F) ((F)->_mp_size < 0 ? -1 : (F)->_mp_size > 0)
#define mpq_sgn(Q) ((Q)->_mp_num._mp_size < 0 ? -1 : (Q)->_mp_num._mp_size > 0)

/* When using GCC, optimize certain common comparisons.  */
#if defined (__GNUC__)
#define mpz_cmp_ui(Z,UI) \
  (__builtin_constant_p (UI) && (UI) == 0				\
   ? mpz_sgn (Z) : _mpz_cmp_ui (Z,UI))
#define mpz_cmp_si(Z,SI) \
  (__builtin_constant_p (SI) && (SI) == 0 ? mpz_sgn (Z)			\
   : __builtin_constant_p (SI) && (SI) > 0				\
    ? _mpz_cmp_ui (Z, (unsigned long int) SI)				\
   : _mpz_cmp_si (Z,SI))
#define mpq_cmp_ui(Q,NUI,DUI) \
  (__builtin_constant_p (NUI) && (NUI) == 0				\
   ? mpq_sgn (Q) : _mpq_cmp_ui (Q,NUI,DUI))
#else
#define mpz_cmp_ui(Z,UI) _mpz_cmp_ui (Z,UI)
#define mpz_cmp_si(Z,UI) _mpz_cmp_si (Z,UI)
#define mpq_cmp_ui(Q,NUI,DUI) _mpq_cmp_ui (Q,NUI,DUI)
#endif


/* Using "&" rather than "&&" means these can come out branch-free.  Every
   mpz_t has at least one limb allocated, so fetching the low limb is always
   allowed.  */
#define mpz_odd_p(z)   ((int) ((z)->_mp_size != 0) & (int) (z)->_mp_d[0])
#define mpz_even_p(z)  (! mpz_odd_p (z))


/* Allow direct user access to numerator and denominator of a mpq_t object.  */
#define mpq_numref(Q) (&((Q)->_mp_num))
#define mpq_denref(Q) (&((Q)->_mp_den))


/* Compatibility with GMP 2 and earlier. */
#define mpn_divmod(qp,np,nsize,dp,dsize) mpn_divrem (qp,0,np,nsize,dp,dsize)

/* Compatibility with GMP 1.  */
#define mpz_mdiv	mpz_fdiv_q
#define mpz_mdivmod	mpz_fdiv_qr
#define mpz_mmod	mpz_fdiv_r
#define mpz_mdiv_ui	mpz_fdiv_q_ui
#define mpz_mdivmod_ui(q,r,n,d) \
  ((r == 0) ? mpz_fdiv_q_ui (q,n,d) : mpz_fdiv_qr_ui (q,r,n,d))
#define mpz_mmod_ui(r,n,d) \
  ((r == 0) ? mpz_fdiv_ui (n,d) : mpz_fdiv_r_ui (r,n,d))

/* Useful synonyms, but not quite compatible with GMP 1.  */
#define mpz_div		mpz_fdiv_q
#define mpz_divmod	mpz_fdiv_qr
#define mpz_div_ui	mpz_fdiv_q_ui
#define mpz_divmod_ui	mpz_fdiv_qr_ui
#define mpz_mod_ui	mpz_fdiv_r_ui
#define mpz_div_2exp	mpz_fdiv_q_2exp
#define mpz_mod_2exp	mpz_fdiv_r_2exp

#define gmp_errno __gmp_errno
extern int gmp_errno;

enum
{
  GMP_ERROR_NONE = 0,
  GMP_ERROR_UNSUPPORTED_ARGUMENT = 1,
  GMP_ERROR_DIVISION_BY_ZERO = 2,
  GMP_ERROR_SQRT_OF_NEGATIVE = 4,
  GMP_ERROR_INVALID_ARGUMENT = 8,
  GMP_ERROR_ALLOCATE = 16,
  GMP_ERROR_BAD_STRING = 32,
  GMP_ERROR_UNUSED_ERROR
};

/* Note: major version number is in mp.h too */
#define __GNU_MP_VERSION 3
#define __GNU_MP_VERSION_MINOR 1
#define __GNU_MP_VERSION_PATCHLEVEL 1

#define gmp_version __gmp_version
extern __gmp_const char *gmp_version;

#define __GMP_H__
#endif /* __GMP_H__ */
