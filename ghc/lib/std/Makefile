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

LIBRARY = libHS$(_way).a
HS_SRCS	= $(wildcard *.lhs)
HS_OBJS = $(HS_SRCS:.lhs=.$(way_)o)
LIBOBJS = $(HS_OBJS)
HS_IFACES= $(HS_SRCS:.lhs=.$(way_)hi) PrelGHC.$(way_)hi


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
PrelArr_HC_OPTS     += -monly-2-regs
Directory_HC_OPTS   += -monly-3-regs 
Time_HC_OPTS        += -monly-3-regs -H16m

# Far too much heap is needed to compile PrelNum with -O at the
# moment, but there you go..
PrelNum_HC_OPTS     += -H30m
# Note: this option has to go in the Makefile rather than in an
# OPTIONS line in the source file. The reason being that we want
# to override the SRC_HC_OPTS of -O, and anything option coming
# from the Makefile overrides what's in OPTIONS lines. (mumble_HC_OPTS
# does override SRC_HC_OPTS settings)
PrelUnsafe_HC_OPTS           += -Onot

PrelBase_HC_OPTS         += -H12m
PrelRead_HC_OPTS         += -H13m
PrelTup_HC_OPTS          += -H12m
PrelArr_HC_OPTS          += -H8m
PrelHandle_HC_OPTS       += -H12m
Time_HC_OPTS             += -H8m
Complex_HC_OPTS          += -H10m
IO_HC_OPTS	         += -H12m

#-----------------------------------------------------------------------------
# 	Dependency generation

SRC_MKDEPENDHS_OPTS += -I$(GHC_INCLUDE_DIR)

#-----------------------------------------------------------------------------
# 	Rules

PrelGHC.$(way_)hi	: PrelGHC.hi-boot
	cp $< $@

boot :: PrelGHC.hi $(foreach way, $(WAYS), PrelGHC.$(way)_hi)

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
INSTALL_LIBS  += $(LIBRARY)
INSTALL_DATAS += $(HS_IFACES)

include $(TOP)/mk/target.mk

glaExts/PackedString_HC_OPTS += -monly-3-regs
concurrent/Parallel_HC_OPTS  += -fglasgow-exts
glaExts/Int_HC_OPTS          += -H8m
glaExts/Word_HC_OPTS         += -H8m
