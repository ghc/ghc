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
HS_SRCS = $(wildcard *.lhs)
HS_OBJS = $(HS_SRCS:.lhs=.$(way_)o)
LIBOBJS = $(HS_OBJS)
HS_IFACES= $(HS_SRCS:.lhs=.$(way_)hi) PrelGHC.$(way_)hi


#-----------------------------------------------------------------------------
# 	Setting the GHC compile options

SRC_HC_OPTS += -recomp -cpp -fglasgow-exts -fvia-C -Rghc-timing $(GhcLibHcOpts) -fcompiling-prelude
ifneq "$(way)" "dll"
SRC_HC_OPTS += -static
endif

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
PrelArrExtra_HC_OPTS     += -monly-2-regs
Directory_HC_OPTS   	 += -monly-3-regs 
Time_HC_OPTS        	 += -monly-3-regs

# Far too much heap is needed to compile PrelNumExtra with -O at the
# moment, but there you go..
PrelNumExtra_HC_OPTS     += -H30m -K2m

PrelPack_HC_OPTS	 += -K4m
PrelBase_HC_OPTS         += -H10m
PrelRead_HC_OPTS         += -H16m
PrelTup_HC_OPTS          += -H12m
PrelNum_HC_OPTS		 += -H12m -K4m
PrelArr_HC_OPTS          += -H8m
PrelHandle_HC_OPTS       += -H14m
Time_HC_OPTS             += -H18m
Complex_HC_OPTS          += -H10m
IO_HC_OPTS	         += -H12m
PrelMain_HC_OPTS	 += -fno-prune-tydecls # avoids an apparent bug; ToDo

#-----------------------------------------------------------------------------
# 	Dependency generation

SRC_MKDEPENDHS_OPTS += -I$(GHC_INCLUDE_DIR)

#-----------------------------------------------------------------------------
# 	Rules

PrelGHC.$(way_)hi	: PrelGHC.hi-boot
	cp $< $@

boot :: PrelGHC.hi $(foreach way, $(WAYS), PrelGHC.$(way)_hi)

DLL_NAME = HSprel.dll
DLL_IMPLIB_NAME = libHS_imp.a
SRC_BLD_DLL_OPTS += --export-all --output-def=HSprel.def
SRC_BLD_DLL_OPTS += -lwinmm -lHSrts_imp -lHS_cbits_imp -lgmp -L. -L../../rts/gmp -L../../rts -Lcbits


ifeq "$(way)" "dll"
HS_SRCS := $(filter-out PrelMain.lhs, $(HS_SRCS))
endif


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
