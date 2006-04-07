## Process this file with automake to generate Makefile.in

# Copyright (C) 1996, 1998, 1999, 2000 Free Software Foundation, Inc.
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


AUTOMAKE_OPTIONS = gnu no-dependencies

SUBDIRS = tests

INCLUDES = -I$(top_srcdir) -DOPERATION_$*

noinst_LTLIBRARIES = libmpz.la
libmpz_la_SOURCES = \
  abs.c add.c add_ui.c addmul_ui.c and.c array_init.c \
  bin_ui.c bin_uiui.c cdiv_q.c \
  cdiv_q_ui.c cdiv_qr.c cdiv_qr_ui.c cdiv_r.c cdiv_r_ui.c cdiv_ui.c \
  clear.c clrbit.c cmp.c cmp_si.c cmp_ui.c cmpabs.c cmpabs_ui.c com.c \
  divexact.c dump.c fac_ui.c fdiv_q.c fdiv_q_2exp.c fdiv_q_ui.c \
  fdiv_qr.c fdiv_qr_ui.c fdiv_r.c fdiv_r_2exp.c fdiv_r_ui.c fdiv_ui.c \
  fib_ui.c fits_sint_p.c fits_slong_p.c fits_sshort_p.c fits_uint_p.c \
  fits_ulong_p.c fits_ushort_p.c gcd.c gcd_ui.c gcdext.c get_d.c get_si.c \
  get_str.c get_ui.c getlimbn.c hamdist.c init.c inp_raw.c inp_str.c \
  invert.c ior.c iset.c iset_d.c iset_si.c iset_str.c iset_ui.c \
  jacobi.c kronsz.c kronuz.c kronzs.c kronzu.c \
  lcm.c legendre.c mod.c mul.c mul_2exp.c neg.c nextprime.c \
  out_raw.c out_str.c perfpow.c perfsqr.c popcount.c pow_ui.c powm.c \
  powm_ui.c pprime_p.c random.c random2.c realloc.c remove.c root.c rrandomb.c \
  scan0.c scan1.c set.c set_d.c set_f.c set_q.c set_si.c set_str.c \
  set_ui.c setbit.c size.c sizeinbase.c sqrt.c sqrtrem.c sub.c \
  sub_ui.c swap.c tdiv_ui.c tdiv_q.c tdiv_q_2exp.c tdiv_q_ui.c tdiv_qr.c \
  tdiv_qr_ui.c tdiv_r.c tdiv_r_2exp.c tdiv_r_ui.c tstbit.c ui_pow_ui.c \
  urandomb.c urandomm.c xor.c

EXTRA_DIST = mul_siui.c
nodist_libmpz_la_SOURCES = mul_si.c mul_ui.c
CLEANFILES = $(nodist_libmpz_la_SOURCES)

mul_si.c: $(srcdir)/mul_siui.c
	cp $(srcdir)/mul_siui.c mul_si.c
mul_ui.c: $(srcdir)/mul_siui.c
	cp $(srcdir)/mul_siui.c mul_ui.c
