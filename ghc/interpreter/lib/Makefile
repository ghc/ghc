# -------------------------------------------------------------------------- #
# $Id: Makefile,v 1.1 1999/11/19 15:33:34 andy Exp $ 
# -------------------------------------------------------------------------- #

TOP = ../..
include $(TOP)/mk/boilerplate.mk

LIBS = List.lhs Ix.lhs Complex.lhs Char.lhs Ratio.lhs Random.lhs \
       Array.lhs Maybe.lhs Monad.lhs Numeric.lhs Directory.lhs \
       System.lhs Locale.lhs CPUTime.lhs IO.lhs Pretty.lhs \
       GetOpt.lhs Bits.lhs Word.lhs Addr.lhs NumExts.lhs Int.lhs 

hugslib :: $(LIBS)


HUGSCPP = ../../utils/hscpp/hscpp -D__HUGS__ -DUSE_REPORT_PRELUDE

%.lhs :: $(GHC_LIB_DIR)/std/%.lhs
	$(HUGSCPP) -I../includes $< > $*.lhs

%.lhs :: $(GHC_LIB_DIR)/exts/%.lhs
	$(HUGSCPP) -I../includes $< > $*.lhs

include $(TOP)/mk/target.mk
