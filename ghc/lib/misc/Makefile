# $Id: Makefile,v 1.19 1999/10/05 10:30:28 simonmar Exp $
#
# Makefile for miscellaneous libraries.
#

TOP = ../..
include $(TOP)/mk/boilerplate.mk

WAYS=$(GhcLibWays)

ifeq "$(way)" ""
SUBDIRS = cbits
else
SUBDIRS=
endif

HC = $(GHC)

#-----------------------------------------------------------------------------
# 	Setting the standard variables
#

LIBRARY = libHSmisc$(_way).a

ifeq "$(EnableWin32DLLs)" "YES"
  HS_SRCS := $(filter-out Select.lhs,$(HS_SRCS))
endif

# Remove Readline.lhs if readline.h isn't available.
ifneq "$(GhcLibsWithReadline)" "YES"
  HS_SRCS := $(filter-out Readline.lhs,$(HS_SRCS))
else
  ifneq "$(ReadlineIncludePath)" ""
     SRC_HC_OPTS += -I$(ReadlineIncludePath)
  endif
endif

HS_OBJS = $(HS_SRCS:.lhs=.$(way_)o)
LIBOBJS = $(HS_OBJS)
HS_IFACES= $(HS_SRCS:.lhs=.$(way_)hi)
SRC_MKDEPENDHS_OPTS += -optdep--include-prelude

#-----------------------------------------------------------------------------
# 	Setting the GHC compile options

SRC_HC_OPTS += -i../concurrent:../posix -recomp -cpp -fglasgow-exts -fvia-C -Rghc-timing $(GhcLibHcOpts)

#
# Profiling options
# (what's this stuff doing here?)
WAY_p_HC_OPTS += -GPrelude
WAY_mr_HC_OPTS += -GPrelude

#
# Object and interface files have suffixes tagged with their ways
#
ifneq "$(way)" ""
SRC_HC_OPTS += -hisuf $(way_)hi
endif

ifneq "$(way)" "dll"
SRC_HC_OPTS += -static
endif

#
# Specific flags
#

BSD_HC_OPTS          += -I../std/cbits -H8m -optc-DNON_POSIX_SOURCE
Socket_HC_OPTS       += -I../std/cbits -optc-DNON_POSIX_SOURCE
SocketPrim_HC_OPTS   += -I../std/cbits -H12m -optc-DNON_POSIX_SOURCE
PackedString_HC_OPTS += -H12m
Native_HC_OPTS       += -H8m
Pretty_HC_OPTS       += -H8m

#-----------------------------------------------------------------------------
# 	Dependency generation

SRC_MKDEPENDHS_OPTS += -I$(GHC_INCLUDE_DIR)

#-----------------------------------------------------------------------------
# 	Win32 DLL setup

DLL_NAME = HSmisc.dll
DLL_IMPLIB_NAME = libHSmisc_imp.a
SRC_BLD_DLL_OPTS += --export-all --output-def=HSmisc.def
SRC_BLD_DLL_OPTS += -lwinmm -lwsock32 -lHSrts_imp -lHScbits_imp -lHSmisc_cbits_imp -lHS_imp -lHSexts_imp -lgmp -L. -L../../rts/gmp -L../../rts -L../std -L../std/cbits -L../exts -Lcbits


#-----------------------------------------------------------------------------
# 	Installation; need to install .hi files as well as libraries
#
# The interface files are put inside the $(libdir), since they
# might (potentially) be platform specific..
#
# override is used here because for binary distributions, datadir is
# set on the command line. sigh.
#
override datadir:=$(libdir)/imports/misc

#
# Files to install from here
# 
INSTALL_LIBS  += $(LIBRARY)
INSTALL_DATAS += $(HS_IFACES)

ifeq "$(EnableWin32DLLs)" "YES"
INSTALL_PROGS += $(DLL_NAME)
INSTALL_LIBS  += $(patsubst %.a, %_imp.a, $(LIBRARY))
INSTALL_DATAS += dLL_ifs.hi
endif

include $(TOP)/mk/target.mk

