# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.7 2001/08/08 13:21:04 qrczak Exp $

TOP=../..
include $(TOP)/mk/boilerplate.mk

HS_PROG        = genprimopcode
SRC_HC_OPTS   += -syslib text

# genprimopcode is needed to boot in ghc/compiler...
ifneq "$(BootingFromHc)" "YES"
boot :: all
endif

include $(TOP)/mk/target.mk
