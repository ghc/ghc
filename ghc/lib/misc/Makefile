#
# Makefile for hslibs subdir
#
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

# Remove Readline.lhs if readline.h isn't available.
ifneq "$(GhcLibsWithReadline)" "YES"
  HS_SRCS := $(filter-out src/Readline.lhs,$(HS_SRCS))
else
  SRC_HC_OPTS += -I$(ReadlineIncludePath)
endif

HS_OBJS = $(HS_SRCS:.lhs=.$(way_)o)
LIBOBJS = $(HS_OBJS)
HS_IFACES= $(HS_SRCS:.lhs=.$(way_)hi) GHC.$(way_)hi


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

#
# Specific flags
#
BSD_HC_OPTS        += -syslib posix -optc-DNON_POSIX_SOURCE
Socket_HC_OPTS     += -I../std/cbits -optc-DNON_POSIX_SOURCE
SocketPrim_HC_OPTS += -I../std/cbits -H10m -syslib posix -optc-DNON_POSIX_SOURCE

#-----------------------------------------------------------------------------
# 	Dependency generation

SRC_MKDEPENDHS_OPTS += -syslib posix -I$(GHC_INCLUDE_DIR)

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

include $(TOP)/mk/target.mk

