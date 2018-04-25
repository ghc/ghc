# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.1 2004/09/14 07:40:51 mthomas Exp $

TOP = ../..
include $(TOP)/mk/boilerplate.mk

#
# Disable 'make boot'
#
NO_BOOT_TARGET=YES

WAYS=

# -----------------------------------------------------------------------------

EXAMPLES    := $(wildcard *.lhs)
BINS        := $(EXAMPLES:.lhs=$(exeext))
CLEAN_FILES += $(BINS)

HC           = $(GHC_INPLACE)
MKDEPENDHS   = $(GHC_INPLACE)
SRC_HC_OPTS += -package Win32

all:: $(BINS)

$(BINS): %$(exeext): %.lhs
	$(HC) -o $@ $(HC_OPTS) $(LD_OPTS) $<

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
