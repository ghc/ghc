# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.1 2001/03/15 15:51:38 simonmar Exp $

TOP=../..
include $(TOP)/mk/boilerplate.mk

HS_PROG           = ghc-pkg
SRC_HC_OPTS      += -cpp -DPKG_TOOL -DWANT_PRETTY -package util -package text -Dlibdir=\"$(libdir)\"

INSTALL_PROGS = $(HS_PROG)

include $(TOP)/mk/target.mk
