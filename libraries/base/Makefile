# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.14 2002/02/12 10:52:47 simonmar Exp $

TOP=..
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------

SUBDIRS = cbits include

ALL_DIRS = \
	Control \
	Control/Concurrent \
	Control/Monad \
	Control/Monad/ST \
	Data \
	Data/Array \
	Database \
	Debug \
	Debug/QuickCheck \
	FileFormat \
	Foreign \
	Foreign/C \
	Foreign/Marshal \
	GHC \
	Hugs \
	Language \
	Network \
	NHC \
	System \
	System/Console \
	System/Mem \
	System/IO \
	Text \
	Text/Html \
	Text/PrettyPrint \
	Text/Regex \
	Text/Show

PACKAGE = base

SRC_HC_OPTS += -fglasgow-exts -cpp -Iinclude
SRC_HSC2HS_OPTS += -Iinclude

# ESSENTIAL, for getting reasonable performance from the I/O library:
GHC/IOBase_HC_OPTS   = -funbox-strict-fields 

# -----------------------------------------------------------------------------
# PrimOpWrappers

GHC/PrimopWrappers.hs: $(GHC_COMPILER_DIR)/prelude/primops.txt
	rm -f $@
	$(GHC_GENPRIMOP) --make-haskell-wrappers < $< > $@

boot :: GHC/PrimopWrappers.hs

CLEAN_FILES += GHC/PrimopWrappers.hs

# -----------------------------------------------------------------------------
# GHC/Prim.hi-boot

GHC/Prim.$(way_)hi	: GHC/Prim.hi-boot
	cp $< $@

ALL_PRIMS = GHC/Prim.hi $(foreach way, $(WAYS), GHC/Prim.$(way)_hi)

lib  : $(ALL_PRIMS)

boot :: $(ALL_PRIMS)

CLEAN_FILES += $(ALL_PRIMS)

SRC_CPP_OPTS += -I$(GHC_INCLUDE_DIR) -traditional

#-----------------------------------------------------------------------------
# 	Building the library for GHCi
#
# The procedure differs from that in fptools/mk/target.mk in one way:
#  (*) on Win32 we must split it into two, because a single .o file can't
#      have more than 65536 relocations in it.

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"

# Turn off standard rule which creates HSbase.o from LIBOBJS.
DONT_WANT_BASE_GHCI_LIB_RULE=YES

GHCI_LIBOBJS = $(HS_OBJS)

HSbase.o : $(GHCI_LIBOBJS)
	$(LD) -r $(LD_X) -o HSbase1.o $(filter     GHC/%, $(GHCI_LIBOBJS))
	$(LD) -r $(LD_X) -o HSbase2.o $(filter-out GHC/%, $(GHCI_LIBOBJS))
	@touch HSbase.o

INSTALL_LIBS += HSbase1.o HSbase2.o

endif # TARGETPLATFORM = i386-unknown-mingw32


# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk

