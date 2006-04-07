#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.6 2001/10/23 16:32:30 rrt Exp $
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

ifneq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
SCRIPT_PROG  	  = ghc-$(ProjectVersion)
LINK 		  = ghc
endif
SCRIPT_OBJS	  = ghc.sh
INTERP		  = $(SHELL)
TOPDIROPT	  = -B$(GHCLIB)
SCRIPT_SUBST_VARS = GHCBIN TOPDIROPT
INSTALL_SCRIPTS  += $(SCRIPT_PROG)

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
