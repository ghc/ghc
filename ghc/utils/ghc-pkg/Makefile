# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.7 2001/03/27 13:38:03 simonmar Exp $

TOP=../..
include $(TOP)/mk/boilerplate.mk

# hack for ghci-inplace script, see below
INSTALLING=1

# -----------------------------------------------------------------------------
# ghc-pkg.bin

HS_PROG           = ghc-pkg.bin
SRC_HC_OPTS      += -cpp -DPKG_TOOL -DWANT_PRETTY -package lang \
		    -package util -package text

INSTALL_LIBEXECS = $(HS_PROG)

# -----------------------------------------------------------------------------
# ghc-pkg script

ifeq "$(INSTALLING)" "1"
ifeq "$(BIN_DIST)"   "1"
GHCPKGBIN=$$\"\"libdir/$(HS_PROG)
PKGCONF=$$\"\"libdir/package.conf
else
GHCPKGBIN=$(libdir)/$(HS_PROG)
PKGCONF=$(libdir)/package.conf
endif # BIN_DIST
else
GHCPKGBIN=$(FPTOOLS_TOP_ABS)/ghc/utils/ghc-pkg/$(HS_PROG)
PKGCONF=$(FPTOOLS_TOP_ABS)/ghc/driver/package.conf.inplace
endif

INSTALLED_SCRIPT_PROG  = ghc-pkg-$(ProjectVersion)
INPLACE_SCRIPT_PROG    = ghc-pkg-inplace

SCRIPT_OBJS	  = ghc-pkg.sh
INTERP		  = $(SHELL)
SCRIPT_SUBST_VARS = GHCPKGBIN PKGCONFOPT
INSTALL_SCRIPTS  += $(SCRIPT_PROG)
PKGCONFOPT 	  = -f $(PKGCONF)

ifeq "$(INSTALLING)" "1"
SCRIPT_PROG 	=  $(INSTALLED_SCRIPT_PROG)
LINK 		=  ghc-pkg
else
SCRIPT_PROG 	=  $(INPLACE_SCRIPT_PROG)
endif

# -----------------------------------------------------------------------------
# don't recurse on 'make install'
#
ifeq "$(INSTALLING)" "1"
all clean distclean maintainer-clean ::
	$(MAKE) INSTALLING=0 BIN_DIST=0 $(MFLAGS) $@
endif

include $(TOP)/mk/target.mk
