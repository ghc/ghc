#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.1 2000/06/13 16:07:21 simonmar Exp $

TOP=../..
include $(TOP)/mk/boilerplate.mk

SCRIPT_PROG 	= ghc-stats
SCRIPT_OBJS     = ghc-stats.prl

INTERP=perl

SCRIPT_SUBST_VARS := TARGETPLATFORM

INSTALL_LIBEXEC_SCRIPTS += $(SCRIPT_PROG)

include $(TOP)/mk/target.mk
