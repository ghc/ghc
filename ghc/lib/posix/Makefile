#
# $Id: Makefile,v 1.7 1999/05/11 17:05:48 keithw Exp $
#
# Makefile for POSIX library
#

TOP=../..
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

LIBRARY = libHSposix$(_way).a
HS_SRCS	= $(wildcard *.lhs)
LIBOBJS = $(HS_OBJS)
HS_IFACES= $(HS_SRCS:.lhs=.$(way_)hi)
SRC_MKDEPENDHS_OPTS += -optdep--include-prelude

#-----------------------------------------------------------------------------
# 	Setting the GHC compile options

SRC_HC_OPTS += -i../misc -recomp -cpp -fglasgow-exts -fvia-C -Rghc-timing $(GhcLibHcOpts)

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
PosixUtil_HC_OPTS     ='-\#include"cbits/libposix.h"' -monly-3-regs
PosixDB_HC_OPTS       ='-\#include"cbits/libposix.h"'
PosixErr_HC_OPTS      ='-\#include"cbits/libposix.h"'
PosixFiles_HC_OPTS    ='-\#include"cbits/libposix.h"'
PosixIO_HC_OPTS       ='-\#include"cbits/libposix.h"'
PosixProcEnv_HC_OPTS  ='-\#include"cbits/libposix.h"'
PosixProcPrim_HC_OPTS ='-\#include"cbits/libposix.h"'
PosixTTY_HC_OPTS      ='-\#include"cbits/libposix.h"' -monly-2-regs
Posix_HC_OPTS         ='-\#include"cbits/libposix.h"'

PosixProcPrim_HC_OPTS += -H8m
PosixFiles_HC_OPTS    += -H8m

# sigh.
../misc/PackedString_HC_OPTS += -H8m

#-----------------------------------------------------------------------------
# 	Dependency generation

SRC_MKDEPENDHS_OPTS += -I$(GHC_INCLUDE_DIR)

#-----------------------------------------------------------------------------
# 	Installation; need to install .hi files as well as libraries
#
# The interface files are put inside the $(libdir), since they
# might (potentially) be platform specific..
#
# override is used here because for binary distributions, datadir is
# set on the command line. sigh.
#
override datadir:=$(libdir)/imports/posix

#
# Files to install from here
# 
INSTALL_LIBS  += $(LIBRARY)
INSTALL_DATAS += $(HS_IFACES)

include $(TOP)/mk/target.mk

