#################################################################################
#
#			    ghc/lib/Makefile
#
#		Makefile for building the GHC Prelude libraries umpteen ways
#
#	
#################################################################################

TOP = ..
include $(TOP)/mk/boilerplate.mk

ifeq "$(way)" ""
SUBDIRS = cbits
ifeq ($(IncludeTestDirsInBuild),YES)
  SUBDIRS += tests
endif
else
SUBDIRS=
endif

#-----------------------------------------------------------------------------
# 	Setting the standard variables
#

LIB_DIRS = ghc required glaExts concurrent

LIBRARY = libHS$(_way).a
HS_SRCS	= $(foreach d, $(LIB_DIRS), $(wildcard $(d)/*.lhs))
HS_OBJS = $(HS_SRCS:.lhs=.$(way_)o)
LIBOBJS = $(HS_OBJS)
HS_IFACES= $(HS_SRCS:.lhs=.$(way_)hi) ghc/GHC.$(way_)hi

#-----------------------------------------------------------------------------
# 	Setting the GHC compile options

SRC_HC_OPTS += -recomp -cpp -fglasgow-exts -fvia-C -Rghc-timing $(GhcLibHcOpts)

#
# Profiling options
WAY_p_HC_OPTS += -GPrelude
WAY_mr_HC_OPTS += -GPrelude

#
# Object and interface files have suffixes tagged with their ways
#
ifneq "$(way)" ""
SRC_HC_OPTS += -hisuf $(way_)hi
endif

# per-module flags
ghc/ArrBase_HC_OPTS	     += -monly-2-regs
glaExts/PackedString_HC_OPTS += -monly-3-regs
required/Directory_HC_OPTS   += -monly-3-regs 
concurrent/Parallel_HC_OPTS  += -fglasgow-exts
required/Time_HC_OPTS        += -monly-3-regs -H12m

# Far too much heap is needed to compile PrelNum with -O at the
# moment, but there you go..
ghc/PrelNum_HC_OPTS          += -H30m
# Note: this option has to go in the Makefile rather than in an
# OPTIONS line in the source file. The reason being that we want
# to override the SRC_HC_OPTS of -O, and anything option coming
# from the Makefile overrides what's in OPTIONS lines. (mumble_HC_OPTS
# does override SRC_HC_OPTS settings)
ghc/UnsafeST_HC_OPTS         += -Onot

ghc/PrelBase_HC_OPTS         += -H8m
ghc/PrelRead_HC_OPTS         += -H8m
ghc/IOHandle_HC_OPTS         += -H8m

#-----------------------------------------------------------------------------
# 	Dependency generation

SRC_MKDEPENDHS_OPTS += -ighc:required:glaExts:concurrent -I$(GHC_INCLUDE_DIR)

#-----------------------------------------------------------------------------
# 	Rules

ghc/GHC.$(way_)hi	: ghc/GHC.hi-boot
	cp $< $@

boot :: ghc/GHC.hi $(foreach way, $(WAYS), ghc/GHC.$(way)_hi)

#-----------------------------------------------------------------------------
# 	Installation; need to install .hi files as well as libraries
#
# The interface files are put inside the $(libdir), since they
# might (potentially) be platform specific..
#
# Note: we use `override' here to ignore the setting of datadir
# which may have been set on the command-line..naughty, as it
# prevents `datadir' from being used from the command-line.
# This only applies to binary-distributions, though.n

ifeq "$(BIN_DIST)" "1"
override datadir:=$(libdir)/imports
else
datadir:=$(libdir)/imports
endif

#
# Files to install from here
# 
INSTALL_LIBS  += $(LIBRARY)
INSTALL_DATAS += $(HS_IFACES)

include $(TOP)/mk/target.mk


