## Process this file with automake to generate Makefile.in


# Copyright (C) 1991, 1993, 1994, 1996, 1997, 1999, 2000 Free Software
# Foundation, Inc.
#
# This file is part of the GNU MP Library.
#
# The GNU MP Library is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation; either version 2.1 of the License, or (at your
# option) any later version.
#
# The GNU MP Library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
# License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with the GNU MP Library; see the file COPYING.LIB.  If not, write to
# the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
# MA 02111-1307, USA.


# make check
#
#     It'd be good if "make check" first did a "make all" or whatever to
#     build libgmp.la, but it's not clear how best to do this.  Putting a
#     "check:" target is overridden by automake, and a "check-local:" runs
#     too late (due to depth-first subdirectory processing).  For now it's
#     necessary to do "make && make check".
#
# MPF_OBJECTS etc
#
#     Libtool needs all the .lo files passed to it if it's going to build
#     both a static and shared library.  If a convenience library like
#     mpf/libmpf.la is passed then the resulting libgmp.a gets the PIC .lo
#     objects rather than the non-PIC .o's.
#
#     Unfortunately this leads to the big lists of objects below.  Something
#     like mpz/*.lo would probably work, but might risk missing something
#     out or getting something extra.  The source files for each .lo are
#     listed in the Makefile.am's in the subdirectories.


# Libtool -version-info for libgmp.la and libmp.la.  See (libtool)Versioning
#
# 1. No interfaces changed, only implementations (good): Increment REVISION.
#
# 2. Interfaces added, none removed (good): Increment CURRENT, increment
#    AGE, set REVISION to 0.
#
# 3. Interfaces removed (BAD, breaks upward compatibility): Increment
#    CURRENT, set AGE and REVISION to 0.
#
# Do this separately for libgmp and libmp, only do it just before a release.
#
#	  GMP	   -version-info
#	release   libgmp  libmp
#	 2.0.x      -       -
#	 3.0      3:0:0   3:0:0
#        3.0.1    3:1:0   3:0:0
#        3.1      4:0:1   4:0:1
#
#
# Starting at 3:0:0 is a slight abuse of the versioning system, but it
# ensures we're past soname libgmp.so.2, which is what has been used on
# Debian GNU/Linux packages of gmp 2.  Pretend gmp 2 was 2:0:0, so the
# interface changes for gmp 3 mean 3:0:0 is right.

LIBGMP_LT_CURRENT =  4
LIBGMP_LT_REVISION = 0
LIBGMP_LT_AGE =      1

LIBMP_LT_CURRENT =  4
LIBMP_LT_REVISION = 0
LIBMP_LT_AGE =      1


AUTOMAKE_OPTIONS = gnu check-news no-dependencies ansi2knr

SUBDIRS = mpn mpz mpq mpf mpbsd mpfr tests demos tune

include_HEADERS = gmp.h $(MPBSD_HEADERS_OPTION) $(MPFR_HEADERS_OPTION)
EXTRA_HEADERS = mp.h

lib_LTLIBRARIES = libgmp.la $(MPBSD_LTLIBRARIES_OPTION)

EXTRA_DIST = .gdbinit gmp-impl.h longlong.h stack-alloc.h urandom.h doc macos

DISTCLEANFILES = asm-syntax.h config.m4 @gmp_srclinks@


MPF_OBJECTS = mpf/init.lo mpf/init2.lo mpf/set.lo mpf/set_ui.lo mpf/set_si.lo \
  mpf/set_str.lo mpf/set_d.lo mpf/set_z.lo mpf/iset.lo mpf/iset_ui.lo \
  mpf/iset_si.lo mpf/iset_str.lo mpf/iset_d.lo mpf/clear.lo mpf/get_str.lo \
  mpf/dump.lo mpf/size.lo mpf/eq.lo mpf/reldiff.lo mpf/sqrt.lo mpf/random2.lo \
  mpf/inp_str.lo mpf/out_str.lo mpf/add.lo mpf/add_ui.lo mpf/sub.lo \
  mpf/sub_ui.lo mpf/ui_sub.lo mpf/mul.lo mpf/mul_ui.lo mpf/div.lo \
  mpf/div_ui.lo mpf/cmp.lo mpf/cmp_ui.lo mpf/cmp_si.lo mpf/mul_2exp.lo \
  mpf/div_2exp.lo mpf/abs.lo mpf/neg.lo mpf/set_q.lo mpf/get_d.lo \
  mpf/set_dfl_prec.lo mpf/set_prc.lo mpf/set_prc_raw.lo mpf/get_prc.lo \
  mpf/ui_div.lo mpf/sqrt_ui.lo mpf/floor.lo mpf/ceil.lo mpf/trunc.lo \
  mpf/pow_ui.lo mpf/urandomb.lo mpf/swap.lo
MPZ_OBJECTS = mpz/abs.lo mpz/add.lo mpz/add_ui.lo mpz/addmul_ui.lo mpz/and.lo \
  mpz/array_init.lo mpz/bin_ui.lo mpz/bin_uiui.lo mpz/cdiv_q.lo \
  mpz/cdiv_q_ui.lo mpz/cdiv_qr.lo mpz/cdiv_qr_ui.lo mpz/cdiv_r.lo \
  mpz/cdiv_r_ui.lo mpz/cdiv_ui.lo mpz/clear.lo mpz/clrbit.lo mpz/cmp.lo \
  mpz/cmp_si.lo mpz/cmp_ui.lo mpz/cmpabs.lo mpz/cmpabs_ui.lo mpz/com.lo \
  mpz/divexact.lo mpz/dump.lo mpz/fac_ui.lo mpz/fdiv_q.lo mpz/fdiv_q_2exp.lo \
  mpz/fdiv_q_ui.lo mpz/fdiv_qr.lo mpz/fdiv_qr_ui.lo mpz/fdiv_r.lo \
  mpz/fdiv_r_2exp.lo mpz/fdiv_r_ui.lo mpz/fdiv_ui.lo mpz/fib_ui.lo \
  mpz/fits_sint_p.lo mpz/fits_slong_p.lo mpz/fits_sshort_p.lo \
  mpz/fits_uint_p.lo mpz/fits_ulong_p.lo mpz/fits_ushort_p.lo mpz/gcd.lo \
  mpz/gcd_ui.lo mpz/gcdext.lo mpz/get_d.lo mpz/get_si.lo mpz/get_str.lo \
  mpz/get_ui.lo mpz/getlimbn.lo mpz/hamdist.lo mpz/init.lo mpz/inp_raw.lo \
  mpz/inp_str.lo mpz/invert.lo mpz/ior.lo mpz/iset.lo mpz/iset_d.lo \
  mpz/iset_si.lo mpz/iset_str.lo mpz/iset_ui.lo mpz/jacobi.lo \
  mpz/kronsz.lo mpz/kronuz.lo mpz/kronzs.lo mpz/kronzu.lo \
  mpz/lcm.lo mpz/legendre.lo \
  mpz/mod.lo mpz/mul.lo mpz/mul_2exp.lo mpz/mul_si.lo mpz/mul_ui.lo \
  mpz/neg.lo mpz/nextprime.lo mpz/out_raw.lo mpz/out_str.lo mpz/perfpow.lo mpz/perfsqr.lo \
  mpz/popcount.lo mpz/pow_ui.lo mpz/powm.lo mpz/powm_ui.lo mpz/pprime_p.lo \
  mpz/random.lo mpz/random2.lo mpz/realloc.lo mpz/remove.lo mpz/root.lo \
  mpz/rrandomb.lo \
  mpz/scan0.lo mpz/scan1.lo mpz/set.lo mpz/set_d.lo mpz/set_f.lo mpz/set_q.lo \
  mpz/set_si.lo mpz/set_str.lo mpz/set_ui.lo mpz/setbit.lo mpz/size.lo \
  mpz/sizeinbase.lo mpz/sqrt.lo mpz/sqrtrem.lo mpz/sub.lo mpz/sub_ui.lo \
  mpz/swap.lo mpz/tdiv_ui.lo mpz/tdiv_q.lo mpz/tdiv_q_2exp.lo mpz/tdiv_q_ui.lo \
  mpz/tdiv_qr.lo mpz/tdiv_qr_ui.lo mpz/tdiv_r.lo mpz/tdiv_r_2exp.lo \
  mpz/tdiv_r_ui.lo mpz/tstbit.lo mpz/ui_pow_ui.lo mpz/urandomb.lo \
  mpz/urandomm.lo mpz/xor.lo
MPQ_OBJECTS = mpq/add.lo mpq/canonicalize.lo mpq/clear.lo mpq/cmp.lo \
  mpq/cmp_ui.lo mpq/div.lo mpq/get_d.lo mpq/get_den.lo mpq/get_num.lo \
  mpq/init.lo mpq/inv.lo mpq/mul.lo mpq/neg.lo mpq/out_str.lo \
  mpq/set.lo mpq/set_den.lo \
  mpq/set_num.lo mpq/set_si.lo mpq/set_ui.lo mpq/sub.lo mpq/equal.lo \
  mpq/set_z.lo mpq/set_d.lo mpq/swap.lo
MPN_OBJECTS = @mpn_objs_in_libgmp@

MPBSD_OBJECTS = mpbsd/add.lo mpbsd/tdiv_qr.lo mpbsd/move.lo mpbsd/powm.lo \
  mpbsd/sub.lo mpbsd/cmp.lo mpbsd/mfree.lo mpbsd/mtox.lo mpbsd/realloc.lo \
  mpbsd/gcd.lo mpbsd/itom.lo mpbsd/min.lo mpbsd/mul.lo mpbsd/mout.lo	  \
  mpbsd/pow_ui.lo mpbsd/sdiv.lo mpbsd/sqrtrem.lo mpbsd/xtom.lo 

# FIXME: Add mpfr/rnd_mode.lo when it's clean.
MPFR_OBJECTS = mpfr/add.lo mpfr/div_2exp.lo mpfr/neg.lo mpfr/set_dfl_prec.lo \
  mpfr/set_str_raw.lo mpfr/agm.lo mpfr/get_str.lo mpfr/print_raw.lo \
  mpfr/set_dfl_rnd.lo mpfr/sqrt.lo mpfr/clear.lo mpfr/init.lo \
  mpfr/set_f.lo mpfr/sub.lo mpfr/cmp.lo mpfr/mul.lo mpfr/round.lo \
  mpfr/set_prec.lo mpfr/cmp_ui.lo mpfr/mul_2exp.lo mpfr/set.lo mpfr/set_si.lo \
  mpfr/div.lo mpfr/mul_ui.lo mpfr/set_d.lo mpfr/pow.lo mpfr/out_str.lo \
  mpfr/pi.lo mpfr/set_z.lo mpfr/add_ulp.lo mpfr/log2.lo mpfr/random.lo \
  mpfr/log.lo mpfr/exp.lo mpfr/div_ui.lo mpfr/zeta.lo mpfr/karadiv.lo \
  mpfr/karasqrt.lo mpfr/print_rnd_mode.lo


if WANT_MPFR
MPFR_HEADERS_OPTION = mpfr/mpfr.h
MPFR_OBJECTS_OPTION = $(MPFR_OBJECTS)
MPFR_LIBADD_OPTION = -lm
endif
libgmp_la_SOURCES = assert.c compat.c errno.c memory.c mp_set_fns.c	 \
  mp_clz_tab.c mp_minv_tab.c						 \
  rand.c randclr.c randlc.c randlc2x.c randraw.c randsd.c		 \
  randsdui.c version.c stack-alloc.c mp_bpl.c extract-dbl.c insert-dbl.c
libgmp_la_DEPENDENCIES = \
  $(MPF_OBJECTS) $(MPZ_OBJECTS) $(MPN_OBJECTS) $(MPQ_OBJECTS) \
  $(MPFR_OBJECTS_OPTION)
libgmp_la_LIBADD = $(libgmp_la_DEPENDENCIES) $(MPFR_LIBADD_OPTION)
libgmp_la_LDFLAGS = \
  -version-info $(LIBGMP_LT_CURRENT):$(LIBGMP_LT_REVISION):$(LIBGMP_LT_AGE)


if WANT_MPBSD
MPBSD_HEADERS_OPTION = mp.h
MPBSD_LTLIBRARIES_OPTION = libmp.la
endif
libmp_la_SOURCES = assert.c errno.c memory.c mp_bpl.c mp_clz_tab.c	\
  mp_minv_tab.c mp_set_fns.c stack-alloc.c
libmp_la_DEPENDENCIES = $(MPBSD_OBJECTS) $(MPN_OBJECTS)			\
  mpz/add.lo mpz/clear.lo mpz/cmp.lo mpz/init.lo mpz/mod.lo mpz/mul.lo	\
  mpz/mul_2exp.lo mpz/realloc.lo mpz/set.lo mpz/set_ui.lo mpz/tdiv_r.lo	\
  mpz/sub.lo
libmp_la_LIBADD = $(libmp_la_DEPENDENCIES)
libmp_la_LDFLAGS = \
  -version-info $(LIBMP_LT_CURRENT):$(LIBMP_LT_REVISION):$(LIBMP_LT_AGE)


info_TEXINFOS = gmp.texi


# Don't ship CVS directories or emacs backups.
dist-hook:
	-find $(distdir) \( -name CVS -type d \) -o -name "*.~*" \
		| xargs rm -rf
