#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.3 2001/03/23 16:36:21 simonmar Exp $

TOP=../..
include $(TOP)/mk/boilerplate.mk

SCRIPT_PROG 	= ghc-asm

INTERP=perl

SCRIPT_SUBST_VARS := TARGETPLATFORM

INSTALL_LIBEXEC_SCRIPTS += $(SCRIPT_PROG)

CLEAN_FILES += $(SCRIPT_OBJS)

# needed for bootstrapping with HC files
ifeq "$(BootingFromHc)" "YES"
boot :: all
endif

include $(TOP)/mk/target.mk
