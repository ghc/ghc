# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.3 2001/03/16 10:07:02 simonmar Exp $

TOP=../..
include $(TOP)/mk/boilerplate.mk

HS_PROG           = ghc-pkg
SRC_HC_OPTS      += -cpp -DPKG_TOOL -DWANT_PRETTY -package lang -package util -package text -Dclibdir='\"$(libdir)\"'

INSTALL_PROGS = $(HS_PROG)

include $(TOP)/mk/target.mk
