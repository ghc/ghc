# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.24 2001/10/12 18:44:52 sof Exp $

TOP=../..
include $(TOP)/mk/boilerplate.mk

CURRENT_DIR=ghc/utils/hsc2hs
INCLUDE_DIR=ghc/includes

INSTALLING=1

HS_PROG           = hsc2hs-bin
SRC_HC_OPTS      += -package util -cpp
ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
SRC_HC_OPTS      += -package win32
endif 

INSTALLED_SCRIPT_PROG  = hsc2hs
INPLACE_SCRIPT_PROG    = hsc2hs-inplace

ifeq "$(INSTALLING)" "1"
TOP_PWD 	:= $(prefix)
SCRIPT_PROG 	=  $(INSTALLED_SCRIPT_PROG)
else
TOP_PWD 	:= $(FPTOOLS_TOP_ABS)
SCRIPT_PROG 	=  $(INPLACE_SCRIPT_PROG)
endif

ifeq "$(INSTALLING)" "1"
ifeq "$(BIN_DIST)"   "1"
HSC2HS_BINDIR=$$\"\"libexecdir
HSC2HS_DIR=$$\"\"libdir
HSC2HS_EXTRA=
else
HSC2HS_BINDIR=$(libexecdir)
HSC2HS_DIR=$(libdir)
HSC2HS_EXTRA=--cc=$(bindir)/ghc-$(ProjectVersion)
endif # BIN_DIST
else
HSC2HS_BINDIR=$(FPTOOLS_TOP_ABS)/$(CURRENT_DIR)
HSC2HS_DIR=$(FPTOOLS_TOP_ABS)/$(CURRENT_DIR)
ifneq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
HSC2HS_EXTRA="--cc=$(CC) --cflag=-D__GLASGOW_HASKELL__=$(ProjectVersionInt) -I$(FPTOOLS_TOP_ABS)/$(INCLUDE_DIR)"
else
extra_flags=$(addprefix --cflag=,$(filter-out -O,$(SRC_CC_OPTS)))
HSC2HS_EXTRA="--cc=$(CC) $(extra_flags) --cflag=-D__GLASGOW_HASKELL__=$(ProjectVersionInt) -I$(FPTOOLS_TOP_ABS)/$(INCLUDE_DIR)"
endif
endif

$(SCRIPT_PROG) : Makefile
$(INSTALLED_SCRIPT_PROG) : $(TOP)/mk/config.mk

SCRIPT_SUBST_VARS = HSC2HS_BINDIR HSC2HS_DIR HS_PROG HSC2HS_EXTRA

SCRIPT_OBJS=hsc2hs.sh
INTERP=$(SHELL)

ifneq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
INSTALL_SCRIPTS  += $(SCRIPT_PROG)
INSTALL_LIBEXECS += $(HS_PROG)
else
INSTALL_PROGS    += $(HS_PROG)
endif

override datadir=$(libdir)
INSTALL_DATAS += template-hsc.h

# -----------------------------------------------------------------------------
# Create driver configuration

CONFIG_HS = Config.hs

boot :: $(CONFIG_HS)

all :: $(CONFIG_HS)

$(CONFIG_HS) : $(FPTOOLS_TOP)/mk/config.mk Makefile
	@$(RM) -f $(CONFIG_HS)
	@echo -n "Creating $(CONFIG_HS) ... "
	@echo "module Config where" >>$(CONFIG_HS)
	@echo "cDEFAULT_TMPDIR       = \"$(DEFAULT_TMPDIR)\"" >> $(CONFIG_HS)
	@echo "cGCC                  = \"$(WhatGccIsCalled)\"" >> $(CONFIG_HS)
	@echo done.

CLEAN_FILES += $(CONFIG_HS)

# don't recurse on 'make install'
#
ifeq "$(INSTALLING)" "1"
all clean distclean maintainer-clean ::
	$(MAKE) INSTALLING=0 BIN_DIST=0 $(MFLAGS) $@
endif

MKDEPENDHS_SRCS += $(CONFIG_HS)

include $(TOP)/mk/target.mk

# hsc2hs-inplace is needed to boot in ghc/lib/std...
ifneq "$(BootingFromHc)" "YES"
boot :: all
endif
