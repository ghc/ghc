#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.10 1997/09/05 09:16:28 simonm Exp $

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
#  for each "way".  The other, libHSclib.a is built once: it is just
#  .lc files that end up the same no matter what, i.e. completely
#  ordinary C.

#  Other sorta independent, compile-once subdirs are:

#	gmp		-- GNU multi-precision library (for Integer)

#-----------------------------------------------------------------------------

TOP=..
DoingRTS=YES
include $(TOP)/mk/boilerplate.mk

WAYS=$(GhcLibWays)

#
# A general rule for the grand mk setup is that in a build tree, only
# directories that don't have any subdirs containing Makefiles are built
# all the different `ways' when doing `make all'. 
#
# The directory structure for the RTS is a bit at odds to the general
# scheme of things, with the GNU gmp library in gmp/ and a way-independent
# archive libHSclib.a beside all the way-archives for the RTS proper.
#
# So to avoid having to redo building the way-proof pieces each time
# the Makefile is invoked with a different setting of $(way), SUBDIRS
# is only set if $(way) isn't set. 
#

SUBDIRS = gmp

#-----------------------------------------------------------------------------
# knock the "clib" (completely ordinary C, compiled once)
# stuff over the head first...
#
# Write out the rule for libHSclib explicitly, as it is special
#  (not to be built n different ways)
#
SRCS_CLIB_LC  = $(wildcard hooks/*.lc) main/Mallocs.lc
LIBOBJS_clib  = $(SRCS_CLIB_LC:.lc=.o)

all :: libHSclib.a

libHSclib.a :: $(LIBOBJS_clib)
	@$(RM) $@
	$(AR) $(AR_OPTS) $@ $(LIBOBJS_clib)
	$(RANLIB) $@

#
# Stuff to clean out, only on way `normal'
#
ifeq "$(way)" ""
MOSTLY_CLEAN_FILES += libHSclib.a $(LIBOBJS_clib)
CLEAN_FILES        += $(SRCS_CLIB_LC:.lc=.c)
endif

#
# Add libHSclib to the install library variable *only*
# if we're doing `make install' the `normal' way - don't want
# to install the same library for each different way.
#
ifeq "$(way)" ""
INSTALL_LIBS += libHSclib.a
endif

#------------------------------------------------------------------
#
# Run-time system parts that are `way' sensitive, you have to build
# a copy of libHSrts for each different ways.
#

SRCS_RTS_LH = $(wildcard storage/*.lh)

SRCS_RTS_LC = $(wildcard c-as-asm/*.lc) \
	gum/GlobAddr.lc			\
	gum/HLComms.lc			\
	gum/Hash.lc			\
	gum/LLComms.lc			\
	gum/Pack.lc			\
	gum/ParInit.lc			\
	gum/RBH.lc			\
	gum/Sparks.lc			\
	gum/Unpack.lc			\
	main/GranSim.lc			\
	main/Itimer.lc			\
	main/Ticky.lc			\
	main/SMRep.lc			\
	main/Select.lc			\
	main/Signals.lc			\
	main/StgOverflow.lc		\
	main/Threads.lc			\
	main/RtsFlags.lc		\
	main/main.lc			\
	prims/PrimArith.lc		\
	prims/PrimMisc.lc		\
	profiling/CostCentre.lc		\
	profiling/Hashing.lc		\
	profiling/HeapProfile.lc	\
	profiling/Indexing.lc		\
	profiling/Timer.lc		\
	storage/SM1s.lc			\
	storage/SM2s.lc			\
	storage/SMap.lc			\
	storage/SMcheck.lc		\
	storage/SMcompacting.lc 	\
	storage/SMcopying.lc 		\
	storage/SMdu.lc			\
	storage/SMevac.lc 		\
	storage/SMextn.lc		\
	storage/SMinit.lc 		\
	storage/SMmarking.lc 		\
	storage/SMscan.lc		\
	storage/SMscav.lc		\
	storage/SMstacks.lc		\
	storage/SMstatic.lc		\
	storage/SMstats.lc 		\
	storage/mprotect.lc

#
# LATER: Include Patrick's generational collector
# that's almost-but-not-quite there: storage/SMgen.lc
#

SRCS_RTS_LHC = $(wildcard main/*.lhc c-as-asm/*.lhc storage/*.lhc gum/*.lhc)

HEADER_FILES = $(SRCS_RTS_LH:.lh=.h)

C_SRCS = $(SRCS_RTS_LC:.lc=.c) $(SRCS_RTS_LHC:.lhc=.hc) $(SRCS_CLIB_LC:.lc=.c) $(HEADER_FILES)

# To avoid having to redo these each time.
.PRECIOUS : %.$(way_)hc

#
# Clean out header files when doing way `normal'
#
ifeq "$(way)" ""
CLEAN_FILES += $(H_FILES) $(C_SRCS)
endif

#-----------------------------------------------------------------------------
# creating and installing libHSrts.a (in its many flavors)
#
LIBRARY = libHSrts$(_way).a
LIBOBJS = $(patsubst %.lc,%.$(way_)o,$(SRCS_RTS_LC)) \
          $(patsubst %.lhc,%.$(way_)o,$(SRCS_RTS_LHC))

SRC_HC_OPTS += -I$(GHC_INCLUDE_DIR) $(GCap) $(GC2s) $(GC1s) -O -optc-DIN_GHC_RTS=1 -I$(GHC_RUNTIME_DIR)/storage 

#
# Note: _have_ to drop the -optc prefix for the GC-type opts (e.g. -optc-DGCap), since
# -o<foo> is interpreted by mkdependC as meaning use <foo> as suffix.
#
# Include -D for all the different collectors to be sure we get at all deps.
#
SRC_MKDEPENDC_OPTS += -I$(GHC_INCLUDE_DIR) -DGCap -DGC1s -DGC2s

#-----------------------------------------------------------------------------
# file-specific options 
c-as-asm/PerformIO_HC_OPTS = -optc-DIN_GHC_RTS=1
gum/FetchMe_HC_OPTS	   = -optc-DIN_GHC_RTS=1
main/StgStartup_HC_OPTS	   = -optc-DIN_GHC_RTS=1
main/StgThreads_HC_OPTS	   = -optc-DIN_GHC_RTS=1
main/StgUpdate_HC_OPTS	   = -optc-DIN_GHC_RTS=1
storage/SMmark_HC_OPTS	   = -optc-DIN_GHC_RTS=1 -optc-DMARK_REG_MAP

#-----------------------------------------------------------------------------
#
# Compiling the individual files
#
# Rules for building various types of objects from C files,
# override the default suffix rule here, as we want to use
# ../driver/ghc (a better C compiler :-) to compile the
# different RTS pieces
#
CC=$(HC) $(HC_OPTS) $($*_HC_OPTS)

#-----------------------------------------------------------------------------
# the TopClosure
#
# Hook it into the list of files to generate dependencies for
#
C_SRCS += main/TopClosure.c

#
# The TopClosure is not part of libHSrts, so we add an extra all::
# target to make sure it is built (TopClosure is way-proof):
#
ifeq "$(way)" ""
all :: main/TopClosure.o

CLEAN_FILES  += main/TopClosure.o
#
# The driver expects to find it in lib/
#
INSTALL_LIBS += main/TopClosure.o
endif

#-----------------------------------------------------------------------------
#
# Files to install
#
# Just libHSrts is installed uniformly across ways
#
INSTALL_LIBS += $(LIBRARY)


#-----------------------------------------------------------------------------
#
# Building the GUM SysMan
#

ifeq "$(way)" "mp"
all :: gum/SysMan

ifdef solaris2_TARGET_OS
__socket_libs = -lsocket -lnsl
else
__socket_libs =
endif

gum/SysMan : gum/SysMan.mp_o gum/LLComms.mp_o main/Mallocs.o hooks/OutOfVM.o
	$(RM) $@
	gcc -o $@ gum/SysMan.mp_o gum/LLComms.mp_o main/Mallocs.o hooks/OutOfVM.o -L$$PVM_ROOT/lib/$$PVM_ARCH -lpvm3 -lgpvm3 $(__socket_libs)

CLEAN_FILES  += gum/SysMan.mp_o gum/SysMan
INSTALL_LIBEXECS += gum/SysMan
endif

include $(TOP)/mk/target.mk
