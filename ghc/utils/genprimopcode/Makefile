# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.6 2001/08/01 20:37:00 qrczak Exp $

TOP=../..
include $(TOP)/mk/boilerplate.mk

HS_PROG        = genprimopcode
SRC_HC_OPTS   += -syslib text
# TODO: remove -H80M when the strictness analyser is working again.
Main_HC_OPTS   = -H80M

# genprimopcode is needed to boot in ghc/compiler...
ifneq "$(BootingFromHc)" "YES"
boot :: all
endif

include $(TOP)/mk/target.mk
