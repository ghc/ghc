# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.5 2001/03/23 16:36:22 simonmar Exp $

TOP=../..
include $(TOP)/mk/boilerplate.mk

HS_PROG        = genprimopcode
SRC_HC_OPTS   += -syslib text

# genprimopcode is needed to boot in ghc/compiler...
ifneq "$(BootingFromHc)" "YES"
boot :: all
endif

include $(TOP)/mk/target.mk
