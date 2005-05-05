#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.11 2005/05/05 00:58:38 sof Exp $
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

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
C_PROG = ghci
C_OBJS += ghci.res
else
C_SRCS=
endif

SCRIPT_OBJS	  = ghci.sh
INTERP		  = $(SHELL)
SCRIPT_SUBST_VARS = GHCBIN TOPDIROPT
ifneq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
INSTALL_SCRIPTS  += $(SCRIPT_PROG)
else
INSTALL_SCRIPTS  += ghcii.sh
INSTALL_PROGS    += $(C_PROG)
endif
TOPDIROPT 	  = -B$(GHCLIB)

ifeq "$(INSTALLING)" "1"
SCRIPT_PROG 	=  $(INSTALLED_SCRIPT_PROG)
ifneq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
LINK 		=  ghci
endif
else
SCRIPT_PROG 	=  $(INPLACE_SCRIPT_PROG)
endif

# don't recurse on 'make install'
#
ifeq "$(INSTALLING)" "1"
all clean distclean maintainer-clean ::
	$(MAKE) INSTALLING=0 BIN_DIST=0 $(MFLAGS) $@
endif

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
ghci.res : ghci.rc ghci.ico
	windres -o ghci.res -i ghci.rc -O coff
endif

include $(TOP)/mk/target.mk
