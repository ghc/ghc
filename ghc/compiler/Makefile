# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.2 1996/11/21 16:46:32 simonm Exp $

TOP = ../..
FlexSuffixRules = YES
YaccSuffixRules = YES
SuffixRule_WantStdOnes = NO
include $(TOP)/ghc/mk/ghc.mk

# Problem: don't know whether GhcWithHscBuiltViaC until now, so we have
# to re-include rules.mk to get the correct suffix rules.

FlexSuffixRules =
YaccSuffixRules =
ifeq ($(GhcWithHscBuiltViaC),YES)
  HC = $(GHC)
  SuffixRule_hc_o = YES
else
  HaskellSuffixRules = YES
endif
include $(TOP)/mk/rules.mk

#-----------------------------------------------------------------------------
# make libhsp.a

YFLAGS = -d
CFLAGS = -Iparser -I. -IcodeGen
ARCHIVE = libhsp.a
DESTDIR =  $(INSTLIBDIR_GHC)
UGN = $(wildcard parser/*.ugn)
UGNC = $(patsubst %.ugn, %.c, $(UGN))
UGNH = $(patsubst %.ugn, %.h, $(UGN))
UGNHS = $(patsubst parser/%.ugn, parser/U_%.hs, $(UGN))
LIBOBJS = \
  $(patsubst %.c, %.o, $(UGNC)) parser/hslexer.o parser/hsparser.tab.o \
  parser/id.o parser/infix.o parser/syntax.o parser/type2context.o \
  parser/util.o

parser/%.h parser/%.c parser/U_%.hs : parser/%.ugn
	@$(RM) $@ parser/$*.hs parser/U_$*.hs parser/$*.h
	$(UGEN) $< || $(RM) parser/$*.h parser/$*.hs
	@$(MV) -f parser/$*.hs parser/U_$*.hs
	@chmod 444 parser/$*.h parser/U_$*.hs

parser/%.o : parser/%.c $(UGNH)
	@$(RM) $@
	$(CC) $(CFLAGS) -c $< -o $@

clean ::
	$(RM) parser/hslexer.c parser/hsparser.tab.h parser/hsparser.tab.c

parser/hslexer.o : parser/hslexer.c parser/hsparser.tab.h

include $(TOP)/mk/lib.mk

#-----------------------------------------------------------------------------

ifeq ($(IncludeTestDirsInBuild),YES)
  SUBDIRS = tests
endif

ifeq ($(Ghc2_0),YES)
  %.hi : %_1_3.lhi
	$(RM) $@
	$(GHC_UNLIT) $<  $@ || ( $(RM) $@ && exit 1 )
	@chmod 444 $@
else
  %.hi : %.lhi
	$(RM) $@
	$(GHC_UNLIT) $<  $@ || ( $(RM) $@ && exit 1 )
	@chmod 444 $@
endif

DIRS = \
  utils basicTypes types hsSyn prelude rename typecheck deSugar coreSyn \
  specialise simplCore stranal stgSyn simplStg codeGen absCSyn main \
  reader profiling parser

# -----------------------------------------------------------------------------
# optional directories

ifeq ($(GhcWithDeforester),YES)
  DIRS += deforest
endif

ifeq ($(GhcWithNativeCodeGen),YES)
  DIRS += nativeGen
else
  ifeq ($(GhcWithHscBuiltViaC),YES)
    # If building via C, we *assume* that it is the distributed C files,
    # which do not have a native-code generator in them
  else
    DIRS += nativeGen
  endif
endif

# -----------------------------------------------------------------------------
# wildcard to get the lists of sources/objects

INCLUDEDIRS = $(foreach dir,$(DIRS),-i$(dir))
SRCS = \
  $(foreach dir,$(DIRS),$(wildcard $(dir)/*.lhs)) \
  $(UGNHS) rename/ParseIface.hs
LOOPS = $(patsubst %.lhi, %.hi, $(wildcard */*.lhi))
HCS = $(patsubst %.hs, %.hc, $(patsubst %.lhs, %.hc, $(SRCS)))
OBJS = \
  $(patsubst %.hc, %.o, $(HCS)) rename/ParseIface.o \
  parser/hsclink.o parser/hschooks.o libhsp.a

# -----------------------------------------------------------------------------
# options for the Haskell compiler

MAKEFLAGS += --no-builtin-rules

HC_OPTS += \
  -cpp $(HcMaxHeapFlag) $(HcMaxStackFlag) -fhaskell-1.3 \
  -fglasgow-exts -DCOMPILING_GHC -Rghc-timing -I. -IcodeGen \
  -InativeGen -Iparser $(INCLUDEDIRS)

# expect 12 shift/reduce conflicts and 0 reduce/reduce conflicts

ifeq ($(GhcWithHscDebug),YES)
  HC_OPTS += -DDEBUG
  CFLAGS += -DDEBUG
endif

ifneq ($(Ghc2_0),YES)
  HC_OPTS += -fomit-derived-read -fomit-reexported-instances
endif

ifeq ($(GhcWithHscOptimised),YES)
  HC_OPTS += -O -fshow-import-specs
endif

ifeq ($(GhcWithDeforester),NO)
  HC_OPTS += -DOMIT_DEFORESTER
endif

# ----------------------------------------------------------------------------

all :: hsc libhsp.a

hsc : $(OBJS)
	$(HC) $(HC_OPTS) -o $@ $^

parser/hschooks.o : parser/hschooks.c
	@$(RM) $@
	$(HC) -c -o $@ $(HCFLAGS) parser/hschooks.c

rename/ParseIface.hs : rename/ParseIface.y
	@$(RM) rename/ParseIface.hs rename/ParseIface.hinfo
	happy -g rename/ParseIface.y
	@chmod 444 rename/ParseIface.hs

# ----------------------------------------------------------------------------
# Special flags for particular modules

absCSyn/AbsCSyn_flags 		= -fno-omit-reexported-instances
hsSyn/HsExpr_flags 		= -K2m
hsSyn/HsSyn_flags 		= -fno-omit-reexported-instances
main/Main_flags 		= -fvia-C
basicTypes/IdInfo_flags 	= -K2m
main/CmdLineOpts_flags 		= -fvia-C
coreSyn/AnnCoreSyn_flags 	= -fno-omit-reexported-instances
nativeGen/PprMach_flags 	= -K2m
parser/UgenAll_flags 		= -fvia-C '-\#include"hspincl.h"'
parser/UgenUtil_flags 		= -fvia-C '-\#include"hspincl.h"'
parser/U_constr_flags 		= -fvia-C '-\#include"hspincl.h"'
parser/U_binding_flags 		= -fvia-C '-\#include"hspincl.h"'
parser/U_pbinding_flags 	= -fvia-C '-\#include"hspincl.h"'
parser/U_entidt_flags 		= -fvia-C '-\#include"hspincl.h"'
parser/U_list_flags 		= -fvia-C '-\#include"hspincl.h"'
parser/U_literal_flags 		= -fvia-C '-\#include"hspincl.h"'
parser/U_maybe_flags 		= -fvia-C '-\#include"hspincl.h"'
parser/U_either_flags 		= -fvia-C '-\#include"hspincl.h"'
parser/U_qid_flags 		= -fvia-C '-\#include"hspincl.h"'
parser/U_tree_flags 		= -fvia-C '-\#include"hspincl.h"'
parser/U_ttype_flags 		= -fvia-C '-\#include"hspincl.h"'
prelude/PrimOp_flags 		= -K3m
reader/ReadPrefix_flags 	= -fvia-C '-\#include"hspincl.h"'
rename/ParseIface_flags 	= -Onot -H16m
rename/RnMonad_flags 		= -fvia-C
rename/RnSource_flags		= -H12m
rename/RnIfaces_flags		= -H8m
rename/RnNames_flags		= -H12m
specialise/Specialise_flags 	= -Onot -H12m
stgSyn/StgSyn_flags 		= -fno-omit-reexported-instances
typecheck/TcMonad_flags 	= -fvia-C
typecheck/TcGenDeriv_flags	= -H10m
typecheck/TcExpr_flags		= -H10m
utils/Argv_flags 		= -fvia-C
utils/CharSeq_flags 		= -fvia-C
utils/SST_flags 		= -fvia-C

#-----------------------------------------------------------------------------
# make depend, clean, tags and install

MKDEPENDHSFLAGS = -f .depend -I../includes -x HsVersions.h
ifeq ($(GhcWithDeforester),NO)
  MKDEPENDHSFLAGS += -DOMIT_DEFORESTER
endif

ifeq ($(GhcWithHscBuiltViaC),YES)
  MKDEPENDHSFLAGS += -o .hc
else
  HS_DEP_SRCS = $(SRCS) # should add $(LOOPS) ?
  include $(TOP)/mk/hsdepend.mk
endif

clean ::
	$(RM) */*.o */*.hi $(UGNC) $(UGNH) $(UGNHS)
	$(RM) rename/ParseIface.hs

veryclean ::
	$(RM) */*.hc

tags ::
	@$(RM) TAGS
	@touch TAGS
	$(HSTAGS) -I../includes $(HSTAGSFLAGS) $(SRCS)

install ::
	$(INSTALL) $(INSTBINFLAGS) hsc $(INSTLIBDIR_GHC)
