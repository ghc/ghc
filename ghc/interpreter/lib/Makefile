# -------------------------------------------------------------------------- #
# $Id: Makefile,v 1.3 1999/11/24 17:25:14 sewardj Exp $ 
# -------------------------------------------------------------------------- #

TOP = ../..
include $(TOP)/mk/boilerplate.mk

LIBS = List.lhs Ix.lhs Complex.lhs Char.lhs Ratio.lhs Random.lhs \
       Array.lhs Maybe.lhs Monad.lhs Numeric.lhs Directory.lhs \
       System.lhs Locale.lhs CPUTime.lhs IO.lhs Pretty.lhs \
       GetOpt.lhs Bits.lhs Word.lhs Addr.lhs NumExts.lhs Int.lhs 

all :: $(LIBS)


HUGSCPP = ../../utils/hscpp/hscpp -D__HUGS__ -DUSE_REPORT_PRELUDE

%.lhs :: $(GHC_LIB_DIR)/std/%.lhs
	$(HUGSCPP) -I../../includes $< > $*.lhs

%.lhs :: $(GHC_LIB_DIR)/exts/%.lhs
	$(HUGSCPP) -I../../includes $< > $*.lhs

CLEAN_FILES += $(LIBS)

include $(TOP)/mk/target.mk

