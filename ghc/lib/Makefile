#################################################################################
#
#			    ghc/lib/Makefile
#
#		Makefile for building the GHC Prelude libraries umpteen ways
#
# 	$Id: Makefile,v 1.12 1997/07/05 01:04:30 sof Exp $
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
WAY_p_HC_OPTS += -prof -GPrelude
WAY_mr_HC_OPTS += -prof -GPrelude

#
# Object and interface files have suffixes tagged with their ways
#
ifneq "$(way)" ""
SRC_HC_OPTS += -hisuf $(way_)hi
endif

# per-module flags

glaExts/PackedString_HC_OPTS += -monly-3-regs
required/Directory_HC_OPTS   += -monly-3-regs 
concurrent/Parallel_HC_OPTS  += -fglasgow-exts
required/Time_HC_OPTS        += -monly-3-regs

# Far too much heap is needed to compile PrelNum with -O at the
# moment, but there you go..
ghc/PrelNum_HC_OPTS          += -H30m

#-----------------------------------------------------------------------------
# 	Dependency generation

SRC_MKDEPENDHS_OPTS += -ighc:required:glaExts:concurrent -I$(GHC_INCLUDE_DIR)

#-----------------------------------------------------------------------------
# 	Rules

## 6/97 - the manual copying of .hi-boot files to .hi is not used
## anymore, renamer will look for a .hi-boot directly.
## ToDo: remove old setup.

ifeq "1" "0"

# In preparation for building the various libHS* libraries,
# we create the interface files needed to boot their build.
#
# Note that the creation of IOBase, Main and GHC interface
# files for umpteen ways is strictly not necessary, they're
# all the same, but having the redundant files prevents us
# from having to treat the said interface files specially
# when creating the dependencies. 
#
# Note: if you change the *.hi-boot files, this will not be
# picked up by the Makefile, you'll have to `make hi-boot'
#
# Note2: hsc will only read from ghc/GHC.hi regardless of 
# which `way' you're compiling, so the copies of the GHC
# interface file for the different ways are only there
# to pacify `make'

HIBOOTS=GHC Main IOBase

# Use variable to control this, so that we can generate
# new boot interface files for new ways without having
# to re-generate the old lot (followed by complete recompile).
#
HIBOOT_WAYS = norm $(WAYS)

hi-boot :
	@for i in $(HIBOOT_WAYS); do \
		echo "Booting interface files for way $$i "; \
		if [ "$$i" != "norm" ]; then \
		   j="$${i}_hi"; \
		else \
		   j='hi'; \
		fi; \
		for ifile in $(HIBOOTS); do \
			echo ".. ghc/$${ifile}.$$j "; \
			cp ghc/$${ifile}.hi-boot ghc/$${ifile}.$${j}; \
		done; \
	done
	@touch ghc/IOBase.lhs
else
hi-boot :
	@echo "Done."
endif

boot :: hi-boot

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


