# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.7 2001/12/21 15:07:20 simonmar Exp $

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
	System/Mem \
	System/IO \
	Text \
	Text/PrettyPrint \
	Text/Regex \
	Text/Show

PKG=core

# -----------------------------------------------------------------------------
# PrimOpWrappers

GHC/PrimopWrappers.hs: $(GHC_COMPILER_DIR)/prelude/primops.txt
	rm -f $@
	$(GHC_GENPRIMOP) --make-haskell-wrappers < $< > $@

boot :: GHC/PrimOpWrappers.hs

CLEAN_FILES += GHC/PrimopWrappers.hs

# -----------------------------------------------------------------------------
# GHC/Prim.hi-boot

#GHC/Prim.$(way_)hi	: GHC/Prim.hi-boot
#	cp $< $@
#
#ALL_PRIMS = GHC/Prim.hi $(foreach way, $(WAYS), GHC/Prim.$(way)_hi)
#
#lib  : $(ALL_PRIMS)
#
#boot :: $(ALL_PRIMS)
#
#CLEAN_FILES += $(ALL_PRIMS)

lib : GHC/Prim.hi-boot

SRC_CPP_OPTS += -I$(GHC_INCLUDE_DIR) -traditional

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk

