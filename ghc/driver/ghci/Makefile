#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.4 2001/06/23 12:54:18 panne Exp $
#

TOP=../..
include $(TOP)/mk/boilerplate.mk

# hack for ghci-inplace script, see below
INSTALLING=1

# -----------------------------------------------------------------------------
# ghci script

ifeq "$(INSTALLING)" "1"
ifeq "$(BIN_DIST)"   "1"
GHCBIN=$$\"\"libexecdir/ghc-$(ProjectVersion)
GHCLIB=$$\"\"libdir
else
GHCBIN=$(libexecdir)/ghc-$(ProjectVersion)
GHCLIB=$(libdir)
endif # BIN_DIST
else
GHCBIN=$(FPTOOLS_TOP_ABS)/ghc/compiler/ghc-$(ProjectVersion)
GHCLIB=$(FPTOOLS_TOP_ABS)
endif

INSTALLED_SCRIPT_PROG  = ghci-$(ProjectVersion)
INPLACE_SCRIPT_PROG    = ghci-inplace

SCRIPT_OBJS	  = ghci.sh
INTERP		  = $(SHELL)
SCRIPT_SUBST_VARS = GHCBIN TOPDIROPT
INSTALL_SCRIPTS  += $(SCRIPT_PROG)
TOPDIROPT 	  = -B$(GHCLIB)

ifeq "$(INSTALLING)" "1"
SCRIPT_PROG 	=  $(INSTALLED_SCRIPT_PROG)
LINK 		=  ghci
else
SCRIPT_PROG 	=  $(INPLACE_SCRIPT_PROG)
endif

# don't recurse on 'make install'
#
ifeq "$(INSTALLING)" "1"
all clean distclean maintainer-clean ::
	$(MAKE) INSTALLING=0 BIN_DIST=0 $(MFLAGS) $@
endif

include $(TOP)/mk/target.mk

