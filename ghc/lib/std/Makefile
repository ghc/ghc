#################################################################################
#
#			    ghc/lib/std/Makefile
#
#		Makefile for building the GHC Prelude libraries umpteen ways
#
#	
#################################################################################

TOP = ../..
include $(TOP)/mk/boilerplate.mk

WAYS=$(GhcLibWays)

ifeq "$(way)" ""
SUBDIRS = cbits
else
SUBDIRS=
endif

#-----------------------------------------------------------------------------
# 	Setting the standard variables
#

HC 	 = $(GHC_INPLACE)
CC	 = $(GHC_INPLACE)

ifneq "$(DLLized)" "YES"
PACKAGE = -package-name std
else
# Hack by SPJ to delay if-then-else until the pattern rule when we have $*
PACKAGE = $(subst ~, ,$(word $(words dummy $(findstring $(notdir $*), PrelMain )), -package-name~std))
endif

HSLIB = std

# we don't want PrelMain in the GHCi library.
GHCI_LIBOBJS = $(filter-out PrelMain.$(way_)o,$(HS_OBJS))

BOOT_SRCS += PrelPrimopWrappers.hs

HSC_SRCS = $(wildcard *.hsc)
HSC_HS   = $(patsubst %.hsc,%.hs, $(HSC_SRCS))
HS_SRCS := $(HSC_HS) $(filter-out $(HSC_HS),$(HS_SRCS))

#-----------------------------------------------------------------------------
# 	Setting the GHC compile options

SRC_HC_OPTS += -cpp -fvia-C -fglasgow-exts $(GhcLibHcOpts) $(PACKAGE)

ifdef USE_REPORT_PRELUDE
SRC_HC_OPTS += -DUSE_REPORT_PRELUDE=1
endif

# Special options
PrelStorable_HC_OPTS = -monly-3-regs
PrelCError_HC_OPTS = +RTS -K4m -RTS

#-----------------------------------------------------------------------------
# 	Dependency generation

SRC_MKDEPENDHS_OPTS += -I$(GHC_INCLUDE_DIR)

#-----------------------------------------------------------------------------
# 	Rules

PrelPrimopWrappers.hs: ../../compiler/prelude/primops.txt
	rm -f PrelPrimopWrappers.hs
	../../utils/genprimopcode/genprimopcode  --make-haskell-wrappers \
		< ../../compiler/prelude/primops.txt > PrelPrimopWrappers.hs

PrelGHC.$(way_)hi	: PrelGHC.hi-boot
	cp $< $@

boot :: PrelPrimopWrappers.hs PrelGHC.hi $(foreach way, $(WAYS), PrelGHC.$(way)_hi)
all :: PrelPrimopWrappers.hs

DLL_DESCRIPTION="GHC-compiled Haskell Prelude"

ifeq "$(DLLized)" "YES"
HS_SRCS := $(filter-out PrelMain.lhs, $(HS_SRCS))
# PrelMain.dll_o isn't to be included in the final .a, 
# but it needs to be generated
all :: PrelMain.dll_o
endif

CLEAN_FILES += PrelGHC.hi $(foreach way, $(WAYS), PrelGHC.$(way)_hi)

CLEAN_FILES +=  \
	$(patsubst %.hsc, %.hs, $(HSC_SRCS)) \
	$(patsubst %.hsc, %.hs.c, $(HSC_SRCS)) \
	$(patsubst %.hsc, %.hs.h, $(HSC_SRCS))

#-----------------------------------------------------------------------------
# 	Installation; need to install .hi files as well as libraries
#
# The interface files are put inside the $(libdir), since they
# might (potentially) be platform specific..
#
# override is used here because for binary distributions, datadir is
# set on the command line. sigh.
#
override datadir:=$(libdir)/imports/std

#
# Files to install from here
# 
ifeq "$(DLLized)" "YES"
INSTALL_LIBS  += PrelMain.dll_o
endif

INSTALL_DATAS += PrelGHC.$(way_)hi

include $(TOP)/mk/target.mk
