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

CPP	= @CPP@

# -DOPERATION_$* tells multi-function files which function to produce.
INCLUDES = -I$(top_srcdir) -DOPERATION_$*

GENERIC_SOURCES = mp_bases.c
OFILES = @mpn_objects@

noinst_LTLIBRARIES = libmpn.la
libmpn_la_SOURCES = $(GENERIC_SOURCES)
libmpn_la_LIBADD = $(OFILES)
libmpn_la_DEPENDENCIES = $(OFILES)

TARG_DIST = a29k alpha arm clipper cray generic hppa i960 lisp m68k m88k \
  mips2 mips3 ns32k pa64 pa64w power powerpc32 powerpc64 pyr sh sparc32 \
  sparc64 thumb vax x86 z8000 z8000x

EXTRA_DIST = underscore.h asm-defs.m4 $(TARG_DIST)

# COMPILE minus CC.  FIXME: Really pass *_CFLAGS to CPP?
COMPILE_FLAGS =	\
	$(DEFS) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS) $(CFLAGS)

SUFFIXES = .s .S .asm

# *.s are not preprocessed at all.
.s.o:
	$(CCAS) $(COMPILE_FLAGS) $<
.s.obj:
	$(CCAS) $(COMPILE_FLAGS) `cygpath -w $<`
.s.lo:
	$(LIBTOOL) --mode=compile $(CCAS) $(COMPILE_FLAGS) $<

# *.S are preprocessed with CPP.
.S.o:
	$(CPP) $(COMPILE_FLAGS) $< | grep -v '^#' >tmp-$*.s
	$(CCAS) $(COMPILE_FLAGS) tmp-$*.s -o $@
	rm -f tmp-$*.s
.S.obj:
	$(CPP) $(COMPILE_FLAGS) `cygpath -w $<` | grep -v '^#' >tmp-$*.s
	$(CCAS) $(COMPILE_FLAGS) tmp-$*.s -o $@
	rm -f tmp-$*.s

# We have to rebuild the static object file without passing -DPIC to
# preprocessor.  The overhead cost is one extra assemblation.  FIXME:
# Teach libtool how to assemble with a preprocessor pass (CPP or m4).

.S.lo:
	$(CPP) $(COMPILE_FLAGS) -DPIC $< | grep -v '^#' >tmp-$*.s
	$(LIBTOOL) --mode=compile $(CCAS) $(COMPILE_FLAGS) tmp-$*.s -o $@
	$(CPP) $(COMPILE_FLAGS) $< | grep -v '^#' >tmp-$*.s
	$(CCAS) $(COMPILE_FLAGS) tmp-$*.s -o $*.o
	rm -f tmp-$*.s

# *.m4 are preprocessed with m4.
.asm.o:
	$(M4) -DOPERATION_$* $< >tmp-$*.s
	$(CCAS) $(COMPILE_FLAGS) tmp-$*.s -o $@
	rm -f tmp-$*.s
.asm.obj:
	$(M4) -DOPERATION_$* `cygpath -w $<` >tmp-$*.s
	$(CCAS) $(COMPILE_FLAGS) tmp-$*.s -o $@
	rm -f tmp-$*.s
.asm.lo:
	$(M4) -DPIC -DOPERATION_$* $< >tmp-$*.s
	$(LIBTOOL) --mode=compile $(CCAS) $(COMPILE_FLAGS) tmp-$*.s -o $@
	$(M4) -DOPERATION_$* $< >tmp-$*.s
	$(CCAS) $(COMPILE_FLAGS) tmp-$*.s -o $*.o
	rm -f tmp-$*.s
