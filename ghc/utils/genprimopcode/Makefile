# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.3 2000/10/23 11:08:22 simonmar Exp $

TOP=../..
include $(TOP)/mk/boilerplate.mk

HS_PROG        = genprimopcode
INSTALL_PROGS += $(HS_PROG)
SRC_HC_OPTS   += -syslib text

# genprimopcode is needed to boot in ghc/compiler...
boot :: all

include $(TOP)/mk/target.mk
