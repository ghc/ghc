#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.29 2000/04/21 18:07:46 panne Exp $

# We create two driver scripts: 
#	- one to run in-place in the build tree for building libraries
#	- one wired to the install locations for later installation
#
# the installation script is built first, and we invoke make recursively
# to build the in-place version.

TOP=..
CURRENT_DIR=ghc/driver
include $(TOP)/mk/boilerplate.mk

INSTALLING=1

DYN_LOADABLE_BITS = \
	ghc-asm.prl \
	ghc-iface.prl \
	ghc-consist.prl \
	ghc-split.prl

INSTALLED_SCRIPT_PROG  = ghc-$(ProjectVersion)
INPLACE_SCRIPT_PROG    = ghc-inplace

ifeq "$(INSTALLING)" "1"
TOP_PWD 	:= $(prefix)
SCRIPT_PROG 	=  $(INSTALLED_SCRIPT_PROG)
SCRIPT_LINK 	=  ghc
else
TOP_PWD 	:= $(FPTOOLS_TOP_ABS)
SCRIPT_PROG 	=  $(INPLACE_SCRIPT_PROG)
endif

SCRIPT_OBJS = ghc.prl
 
INTERP=perl

#
# The driver needs to know the options and names for 
# all possible ways, so we magically generate the
# the make variable names for them here.
#

USER_WAY_NAMES = $(foreach way,$(USER_WAYS),WAY_$(way)_NAME)
USER_WAY_OPTS  = $(foreach way,$(USER_WAYS),WAY_$(way)_REAL_OPTS)

SCRIPT_SUBST_VARS := \
  INSTALLING \
  ProjectName ProjectVersion ProjectVersionInt \
  HscMajorVersion HscMinorVersion CcMajorVersion CcMinorVersion \
  CURRENT_DIR HOSTPLATFORM TARGETPLATFORM \
  GHC_LIB_DIR GHC_RUNTIME_DIR GHC_INCLUDE_DIR \
  GHC_OPT_HILEV_ASM GhcWithNativeCodeGen LeadingUnderscore\
  GHC_UNLIT GHC_HSCPP GHC_MKDEPENDHS GHC_HSC GHC_SYSMAN EnableWin32DLLs \
  CP RM CONTEXT_DIFF LibGmp GhcWithRegisterised LibsReadline \
  $(USER_WAY_NAMES) $(USER_WAY_OPTS)

#
# When creating a binary distribution, we prefix the driver script
# with a short msg about what variables need to be set to get the
# script going. 
#

ifeq "$(BIN_DIST)" "1"
SCRIPT_PREFIX_FILES=prefix.txt
else
SCRIPT_SUBST_VARS += libdir libexecdir datadir bindir TMPDIR TOP_PWD
endif

all :: $(DYN_LOADABLE_BITS)

# don't recurse on 'make install'
#
ifeq "$(INSTALLING)" "1"
all clean veryclean maintainer-clean ::
	$(MAKE) INSTALLING=0 BIN_DIST=0 $(MFLAGS) $@
endif

#
# Install setup:
#   the driver goes in $(bindir), the perl script helpers
#   in $(libdir)
#
# ToDo: allow different install name for driver?
#
INSTALL_SCRIPTS += $(SCRIPT_PROG)
INSTALL_LIBS    += $(DYN_LOADABLE_BITS)

#
# depend setup: other directories need the driver script to compute
# their dependencies, so `depend' is simply an alias for `all' here.
depend :: all

#
# Clean up
#
CLEAN_FILES += $(SCRIPT_OBJS) $(SCRIPT_LINK) $(DYN_LOADABLE_BITS) ghc-[0-9].*

#
# Source distribution
#
SRC_DEST_FILES=$(patsubst %.prl,%.lprl,$(DYN_LOADABLE_BITS)) ghc.lprl ordering-passes test_mangler

include $(TOP)/mk/target.mk

ghc.prl : $(TOP)/mk/version.mk

#
# Another hack (hmm..I can see a pattern developing here :-)
# In ghc/driver, we create a symlink from ghc-<whatever-version> to
# ghc, but we don't want this included in a source distribution.
# We `solve' this by removing `ghc' from the dist tree here.
dist ::
	@echo "Patching dist tree: removing $(SRC_DIST_DIR)/ghc symlink"
	$(RM) $(SRC_DIST_DIR)/ghc
