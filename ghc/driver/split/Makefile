#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.2 2000/11/03 16:54:52 simonmar Exp $

TOP=../..
include $(TOP)/mk/boilerplate.mk

SCRIPT_PROG 	= ghc-split

INTERP=perl

SCRIPT_SUBST_VARS := TARGETPLATFORM

INSTALL_LIBEXEC_SCRIPTS += $(SCRIPT_PROG)

CLEAN_FILES += $(SCRIPT_OBJS)

include $(TOP)/mk/target.mk
