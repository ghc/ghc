#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.19 1999/11/22 11:54:36 simonmar Exp $

TOP=../..
include $(TOP)/mk/boilerplate.mk

INSTALLING=1

boot :: all

SCRIPT_SUBST_VARS= \
 TOP_PWD \
 INSTALLING \
 RAWCPP \
 HscIfaceFileVersion

INTERP=perl

INSTALLED_SCRIPT_PROG  = mkdependHS
INPLACE_SCRIPT_PROG    = mkdependHS-inplace

ifeq "$(INSTALLING)" "1"
TOP_PWD 	:= $(prefix)
SCRIPT_PROG 	=  $(INSTALLED_SCRIPT_PROG)
SCRIPT_LINK 	=  ghc
else
TOP_PWD 	:= $(FPTOOLS_TOP_ABS)
SCRIPT_PROG 	=  $(INPLACE_SCRIPT_PROG)
endif

SCRIPT_OBJS=mkdependHS.prl

ifeq "$(BIN_DIST)" "1"
SCRIPT_PREFIX_FILES += prefix.txt
else
SCRIPT_SUBST_VARS += libdir libexecdir datadir TMPDIR SED
endif

# don't recurse on 'make install'
#
ifeq "$(INSTALLING)" "1"
all clean veryclean maintainer-clean ::
	$(MAKE) INSTALLING=0 BIN_DIST=0 $(MFLAGS) $@
endif

#
# install setup
#
INSTALL_LIB_SCRIPTS += $(SCRIPT_PROG)

include $(TOP)/mk/target.mk
