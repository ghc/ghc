#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.5 2001/06/27 15:26:56 chak Exp $
#

TOP=../..
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------
# ghc script

ifeq "$(BIN_DIST)"   "1"
GHCBIN=$$\"\"libexecdir/ghc-$(ProjectVersion)
GHCLIB=$$\"\"libdir
else
GHCBIN=$(libexecdir)/ghc-$(ProjectVersion)
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
