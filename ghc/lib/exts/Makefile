#################################################################################
#
#			    ghc/lib/Makefile
#
#		Makefile for building the GHC Prelude libraries umpteen ways
#
#	
#################################################################################

TOP = ../..
include $(TOP)/mk/boilerplate.mk

WAYS=$(GhcLibWays)

#-----------------------------------------------------------------------------
# 	Setting the standard variables
#

LIBRARY = libHSexts$(_way).a
HS_SRCS	= $(wildcard *.lhs)
HS_OBJS = $(HS_SRCS:.lhs=.$(way_)o)
LIBOBJS = $(HS_OBJS)
HS_IFACES= $(HS_SRCS:.lhs=.$(way_)hi)

#-----------------------------------------------------------------------------
# 	Setting the GHC compile options

SRC_HC_OPTS += -recomp -cpp -fglasgow-exts -fvia-C -Rghc-timing $(GhcLibHcOpts)

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

Int_HC_OPTS          += -H14m -fno-prune-tydecls -monly-3-regs
Word_HC_OPTS         += -H12m -monly-3-regs
Foreign_HC_OPTS      += -fno-prune-tydecls
NativeInfo_HC_OPTS   += -fno-prune-tydecls
Dynamic_HC_OPTS	     += $(MAGIC_HSCPP_OPTS)

MAGIC_HSCPP_OPTS=-DBEGIN_FOR_GHC='-}' -DEND_FOR_GHC='{-' -DBEGIN_FOR_HUGS='{-' -DEND_FOR_HUGS='-}'

#-----------------------------------------------------------------------------
# 	Dependency generation

SRC_MKDEPENDHS_OPTS += -I$(GHC_INCLUDE_DIR) -optdep--include-prelude -optdep-w $(MAGIC_HSCPP_OPTS)

#-----------------------------------------------------------------------------
# 	Win32 DLL setup

DLL_NAME = HSexts.dll
DLL_IMPLIB_NAME = libHSexts_imp.a
SRC_BLD_DLL_OPTS += --export-all --output-def=HSexts.def
SRC_BLD_DLL_OPTS += -lwinmm -lHSrts_imp -lHS_cbits_imp -lHS_imp -lgmp -L. -L../../rts/gmp -L../../rts -L../std -L../std/cbits

#-----------------------------------------------------------------------------
# 	Installation; need to install .hi files as well as libraries
#
# The interface files are put inside the $(libdir), since they
# might (potentially) be platform specific..
#
# override is used here because for binary distributions, datadir is
# set on the command line. sigh.
#
override datadir:=$(libdir)/imports/exts

#
# Files to install from here
# 
INSTALL_LIBS  += $(LIBRARY)
INSTALL_DATAS += $(HS_IFACES)

include $(TOP)/mk/target.mk
