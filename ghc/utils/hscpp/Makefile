#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.11 1999/10/05 10:30:33 simonmar Exp $
#
# hscpp doesn't depend on any of the install paths, so we don't need to
# play the same tricks we play with mkdependHS and the GHC driver script.

TOP=../..
include $(TOP)/mk/boilerplate.mk

SCRIPT_PROG=hscpp
SCRIPT_OBJS=hscpp.prl
SCRIPT_SUBST_VARS= RAWCPP

INTERP=perl

#
# install setup
#
INSTALL_LIB_SCRIPTS+=$(SCRIPT_PROG)
INSTALL_LIBEXECS=$(C_PROG)

include $(TOP)/mk/target.mk
