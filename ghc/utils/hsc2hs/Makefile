# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.7 2001/02/05 18:01:39 rrt Exp $

TOP=../..
include $(TOP)/mk/boilerplate.mk

CURRENT_DIR=ghc/utils/hsc2hs
INCLUDE_DIR=ghc/includes

INSTALLING=1

HS_PROG           = hsc2hs-bin
SRC_HC_OPTS      += -package util -package text -cpp -fglasgow-exts

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
HSC2HS_DIR=$$\"\"libdir
HSC2HS_EXTRA=
else
HSC2HS_DIR=$(libdir)
HSC2HS_EXTRA=--cc=$(bindir)/ghc-$(ProjectVersion)
endif # BIN_DIST
else
HSC2HS_DIR=$(FPTOOLS_TOP_ABS)/$(CURRENT_DIR)
HSC2HS_EXTRA="--cc=$(CC) --cflag=-D__GLASGOW_HASKELL__=$(ProjectVersionInt) -I$(FPTOOLS_TOP_ABS)/$(INCLUDE_DIR)"
endif

$(SCRIPT_PROG) : Makefile
$(INSTALLED_SCRIPT_PROG) : $(TOP)/mk/config.mk

SCRIPT_SUBST_VARS = HSC2HS_DIR HS_PROG HSC2HS_EXTRA

SCRIPT_OBJS=hsc2hs.sh
INTERP=$(SHELL)

INSTALL_SCRIPTS  += $(SCRIPT_PROG)
INSTALL_LIBEXECS += $(HS_PROG)

override datadir=$(libdir)
INSTALL_DATAS += template-hsc.h

# don't recurse on 'make install'
#
ifeq "$(INSTALLING)" "1"
all clean veryclean maintainer-clean ::
	$(MAKE) INSTALLING=0 BIN_DIST=0 $(MFLAGS) $@
endif

# hsc2hs-inplace is needed to boot in ghc/lib/std...
boot :: all

include $(TOP)/mk/target.mk
