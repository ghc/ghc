# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.8 2002/09/09 11:44:13 simonmar Exp $

TOP=../..
include $(TOP)/mk/boilerplate.mk

HS_PROG        = genprimopcode

ghc_ge_504 = $(shell if (test $(GhcCanonVersion) -ge 504); then echo YES; else echo NO; fi)

ifeq "$(ghc_ge_504)" "NO"
SRC_HC_OPTS +=  -package text
endif

# genprimopcode is needed to boot in ghc/compiler...
ifneq "$(BootingFromHc)" "YES"
boot :: all
endif

include $(TOP)/mk/target.mk
