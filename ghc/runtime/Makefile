#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.2 1996/11/21 16:48:17 simonm Exp $

#  This is the Makefile for the runtime-system stuff.
#  This stuff is written in C (and cannot be written in Haskell).
#
#  Things are organised into exactly one level of subdirs.
#
#  At the moment, there are a few such subdirs:
#	c-as-asm	-- mini-interpreter & register hackery
#	gum		-- GUM-specific stuff
#	main		-- "main", associated startup stuff, & MISC things
#	prims		-- code for primitives that must be written in C
#	profiling	-- cost-centre profiling
#	storage		-- the storage manager(s)
#
#  We create two libraries.  One, libHSrts<tag>.a, is built separately
#  for each "build".  The other, libHSclib.a is built once: it is just
#  .lc files that end up the same no matter what, i.e. completely
#  ordinary C.

#  Other sorta independent, compile-once subdirs are:

#	gmp		-- GNU multi-precision library (for Integer)

#-----------------------------------------------------------------------------

TOP=../..
SuffixRules_WantStdOnes = NO
UnlitSuffixRules = YES_PLEASE
include $(TOP)/ghc/mk/ghc.mk

SUBDIRS = gmp
include $(TOP)/mk/subdir.mk

# If you use EXTRA_HC_OPTS on the command line (which you shouldn't,
# strictly speaking), it will probably work -- it is pinned onto
# GHC_OPTS, just for fun.

GHC_OPTS = $(EXTRA_HC_OPTS)

# per-build options: shared with libraries
DoingRTS = YES
include ../mk/buildflags.mk

#-----------------------------------------------------------------------------
# knock the "clib" (completely ordinary C, compiled once)
# stuff over the head first...

CLIB_LC = 				\
	hooks/ErrorHdr.lc		\
	hooks/FreeForeignObj.lc		\
	hooks/OutOfHeap.lc		\
	hooks/OutOfStk.lc		\
	hooks/OutOfVM.lc		\
	hooks/NoRunnableThrds.lc	\
	hooks/PatErrorHdr.lc		\
	hooks/TraceHooks.lc		\
	hooks/SizeHooks.lc		\
	hooks/InitEachPE.lc		\
	main/Mallocs.lc

ARCHIVE = libHSclib.a
LIBOBJS = $(CLIB_LC:.lc=.o)
DESTDIR	= $(INSTLIBDIR_GHC)

include $(TOP)/mk/lib.mk

#-----------------------------------------------------------------------------
# Rules for building various types of objects from C files

RTS_GHC = $(GHC) -c -o $@ $(GHCFLAGS) $($*_flags) 

%.o : %.c
	@$(RM) $@
	$(RTS_GHC) $*.c

#-----------------------------------------------------------------------------
# the TopClosure

all :: main/TopClosure.o

clean ::
	$(RM) main/TopClosure.o

install :: main/TopClosure.o
	$(INSTALL) $(INSTLIBFLAGS) main/TopClosure.o $(INSTLIBDIR_GHC)/TopClosure.o

#-----------------------------------------------------------------------------
# libHSrts.a, in several different 'ways'

all ::
	@for i in $(WAY_SUFFIXES); do \
		echo; \
		echo =========== Making runtime system for way $$i; \
		echo; \
		$(MAKE) -f Makefile.libHSrts suffix=$$i; \
	done

install ::
	@for i in $(WAY_SUFFIXES); do \
		$(MAKE) -f Makefile.libHSrts suffix=$$i install; \
	done

clean ::
	@$(MAKE) -f Makefile.libHSrts clean
	@for i in $(WAY_SUFFIXES); do \
		$(MAKE) -f Makefile.libHSrts suffix=$$i cleanobj; \
	done

# Dependencies are done by Makefile.libHSrts

depend ::
	@$(MAKE) -f Makefile.libHSrts depend
