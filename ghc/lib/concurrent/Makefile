# $Id: Makefile,v 1.6 1999/05/05 10:48:06 sof Exp $
#
# Makefile for concurrent libraries.
#

TOP = ../..
include $(TOP)/mk/boilerplate.mk

WAYS=$(GhcLibWays)

#-----------------------------------------------------------------------------
# 	Setting the standard variables
#

LIBRARY = libHSconcurrent$(_way).a
HS_SRCS	= $(wildcard *.lhs)
HS_OBJS = $(HS_SRCS:.lhs=.$(way_)o)
LIBOBJS = $(HS_OBJS)
HS_IFACES= $(HS_SRCS:.lhs=.$(way_)hi)


#-----------------------------------------------------------------------------
# 	Setting the GHC compile options

SRC_HC_OPTS += -recomp -cpp -fglasgow-exts -fvia-C -Rghc-timing $(GhcLibHcOpts)
SRC_MKDEPENDHS_OPTS += -optdep--include-prelude

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

Parallel_HC_OPTS  += -fglasgow-exts

#-----------------------------------------------------------------------------
# 	Dependency generation

SRC_MKDEPENDHS_OPTS += -I$(GHC_INCLUDE_DIR)

DLL_NAME = HSconc.dll
DLL_IMPLIB_NAME = libHSconcurrent_imp.a
SRC_BLD_DLL_OPTS += --export-all --output-def=HSconc.def
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
override datadir:=$(libdir)/imports/concurrent

#
# Files to install from here
# 
INSTALL_LIBS  += $(LIBRARY)
INSTALL_DATAS += $(HS_IFACES)

include $(TOP)/mk/target.mk

