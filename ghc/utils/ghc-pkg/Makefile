# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.12 2001/10/12 14:28:46 rrt Exp $

TOP=../..
include $(TOP)/mk/boilerplate.mk

# hack for ghci-inplace script, see below
INSTALLING=1

# -----------------------------------------------------------------------------
# ghc-pkg.bin

HS_PROG           = ghc-pkg.bin
SRC_HC_OPTS      += -cpp -DPKG_TOOL -DWANT_PRETTY -package lang \
		    -package util -package text

ifneq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
INSTALL_LIBEXECS += $(HS_PROG)
else
INSTALL_PROGS    += $(HS_PROG)
SRC_HC_OPTS      += -package win32
endif


# -----------------------------------------------------------------------------
# ghc-pkg script

ifeq "$(INSTALLING)" "1"
ifeq "$(BIN_DIST)"   "1"
GHCPKGBIN=$$\"\"libexecdir/$(HS_PROG)
PKGCONF=$$\"\"libdir/package.conf
else
GHCPKGBIN=$(libexecdir)/$(HS_PROG)
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
ifneq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
INSTALL_SCRIPTS  += $(SCRIPT_PROG)
endif
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
