# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.12 2002/02/11 17:10:56 simonmar Exp $

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

PKG = base

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

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk

