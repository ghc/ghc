#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.2 2001/03/26 22:42:42 qrczak Exp $
#

TOP=../..
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------
# ghc script

ifeq "$(BIN_DIST)"   "1"
GHCBIN=$$\"\"libdir/ghc-$(ProjectVersion)
GHCLIB=$$\"\"libdir
else
GHCBIN=$(libdir)/ghc-$(ProjectVersion)
GHCLIB=$(libdir)
endif # BIN_DIST

SCRIPT_PROG  	  = ghc-$(ProjectVersion)
SCRIPT_OBJS	  = ghc.sh
INTERP		  = $(SHELL)
TOPDIROPT	  = -B$(GHCLIB)
SCRIPT_SUBST_VARS = GHCBIN TOPDIROPT
LINK 		  = ghc
INSTALL_SCRIPTS  += $(SCRIPT_PROG)

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
