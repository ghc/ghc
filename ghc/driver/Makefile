#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.2 1996/11/21 16:47:27 simonm Exp $

TOP=../..
CURRENT_DIR=ghc/driver
UnlitSuffixRules = YES
include $(TOP)/ghc/mk/ghc.mk

DYN_LOADABLE_BITS = \
	ghc-asm.prl \
	ghc-recomp.prl \
	ghc-iface.prl \
	ghc-consist.prl \
	ghc-split.prl

PROG	= ghc
SRC	= ghc.prl
INTERP	= $(PERL)
DESTDIR	= $(INSTBINDIR_GHC)
INSTALLED_NAME = $(GHC_DRIVER_INST_NAME)

all :: $(DYN_LOADABLE_BITS)

install :: $(DYN_LOADABLE_BITS)
	$(INSTALL) $(INSTDATAFLAGS) $(DYN_LOADABLE_BITS) $(INSTLIBDIR_GHC)

clean::
	$(RM) ghc.prl
	$(RM) $(DYN_LOADABLE_BITS)

# DYN_LOADABLE_LPRLS = $(DYN_LOADABLE_BITS:.prl=.lprl)
# PerlTagsTarget( ghc.lprl $(DYN_LOADABLE_LPRLS) )

include $(TOP)/mk/script.mk
