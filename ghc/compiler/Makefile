# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.13 1997/05/19 00:22:33 sof Exp $

TOP = ..
include $(TOP)/mk/boilerplate.mk

#-----------------------------------------------------------------------------
# Building hsc different ways (default is just `normal' sequential)
override WAYS=$(GhcCompilerWays)

#-----------------------------------------------------------------------------
#		Set SUBDIRS

ifeq ($(IncludeTestDirsInBuild),YES)
  SUBDIRS = tests
endif

# -----------------------------------------------------------------------------
#		Set HS_PROG, LIBRARY
# Setting HS_PROG and LIBRARY causes all targets in target.mk
# (included below) to kick in.

LIBRARY=libhsp.a

#
# Note: there have been reports of people running up against the ARG_MAX limit
# when linking hsc with all its constituent object files. The likely source of 
# the problem is that the environment is a bit too big, so a workaround could
# be to do ` env PATH=$(PATH) make hsc 'to minimise the environment. (or the
# equivalent of `env' if it doesn't exist locally).
#
HS_PROG=hsc


# -----------------------------------------------------------------------------
# 		Set SRCS, LOOPS, HCS, OBJS
#
# First figure out DIRS, the source sub-directories
# Then derive SRCS by looking in them
#

DIRS = \
  utils basicTypes types hsSyn prelude rename typecheck deSugar coreSyn \
  specialise simplCore stranal stgSyn simplStg codeGen absCSyn main \
  reader profiling parser


ifeq ($(GhcWithDeforester),YES)
  DIRS += deforest
endif

ifeq ($(GhcWithNativeCodeGen),YES)
  ifeq ($(GhcWithHscBuiltViaC),YES)
    # If building via C, we *assume* that it is the distributed C files,
    # which do not have a native-code generator in them
    SRC_MKDEPENDHS_OPTS += -DOMIT_NATIVE_CODEGEN
  else
    DIRS += nativeGen
  endif
else
SRC_HC_OPTS += -DOMIT_NATIVE_CODEGEN
endif


HS_SRCS = $(SRCS_UGNHS) \
          $(foreach dir,$(DIRS),$(wildcard $(dir)/*.lhs)) \
	  rename/ParseIface.hs rename/ParseType.hs rename/ParseUnfolding.hs \
	  main/LoopHack.hc 

# NB: it's no good to include *.hs in the top-line wildcard, because the .hs files
#     in parser/ may not have been created at that point.


HCS      = $(patsubst %.lhs, %.hc, $(patsubst %.hs, %.hc, $(HS_SRCS)))

#
# When booting using a ghc-2.xx compiler, the loop breakers are actually written in
# Haskell, source stubs just re-exporting info from the various .hi-boot files.
# (Reason: we don't want to fiddle with the imports in the sources). We give them
# the suffix .hs, since they're special and don't fall under the umbrella of $(HS_SRCS)
# always (only when $(Ghc2_0) is equal to YES)
ifeq "$(Ghc2_0)" "YES"
LOOPS_HS  = $(wildcard */*Loop*.hs)
MKDEPENDHS_SRCS = $(HS_SRCS)
#HS_SRCS  += $(LOOPS_HS)
else
LOOPS   = $(patsubst %.lhi, %.hi, $(wildcard */*.lhi))
endif
 
HS_OBJS  = \
  $(patsubst %.hc, %.o, $(HCS)) \
  parser/hsclink.o parser/hschooks.o libhsp.a


DESTDIR         = $(INSTALL_LIBRARY_DIR_GHC)

SRCS_UGN      = $(wildcard parser/*.ugn)
SRCS_UGNC     = $(patsubst %.ugn, %.c, $(SRCS_UGN))
SRCS_UGNH     = $(patsubst %.ugn, %.h, $(SRCS_UGN))
SRCS_UGNHS    = $(patsubst parser/%.ugn, parser/U_%.hs, $(SRCS_UGN))
SRCS_UGN_OBJS = $(patsubst %.c, %.o, $(SRCS_UGNC))

#
# Add the generated C files to the C_SRCS, so as to let `depend'
# generate correct dependencies for them.
#
C_SRCS += $(SRCS_UGNC)

LIBOBJS = \
  $(SRCS_UGN_OBJS) parser/hslexer.o parser/hsparser.tab.o \
  parser/id.o parser/infix.o parser/syntax.o parser/type2context.o \
  parser/util.o

#
# stuff you get for free in a source distribution
# 
SRC_DIST_FILES += rename/ParseIface.hs \
 rename/ParseType.hs rename/ParseUnfolding.hs \
 parser/U_tree.c parser/tree.h parser/tree.c \
 parser/hsparser.tab.c parser/hsparser.tab.h \
 parser/hslexer.c

# -----------------------------------------------------------------------------
# 		Haskell compilations

# Compiler to use for building hsc
#
HC=$(WithGhcHc)

SRC_HC_OPTS += \
  -cpp -fhaskell-1.3 \
  -fglasgow-exts -DCOMPILING_GHC -Rghc-timing -I. -IcodeGen \
  -InativeGen -Iparser $(foreach dir,$(DIRS),-i$(dir))

# -syslib ghc just needed for use of PackedString.hPutPS


SRC_CC_OPTS += -Iparser -I.

ifeq "$(Ghc2_0)" "NO"
SRC_HC_OPTS += -fomit-derived-read -fomit-reexported-instances
else
# Recompilation checking - unconditionally so (ToDo: move to mk/config.mk.in)
SRC_HC_OPTS += -recomp
endif

ifeq ($(GhcWithDeforester),NO)
ifeq "$(Ghc2_0)" "NO"
  SRC_MKDEPENDHS_OPTS += -DOMIT_DEFORESTER
endif
SRC_HC_OPTS += -DOMIT_DEFORESTER
endif

SRC_HC_OPTS += $(GhcHcOpts)

# 	Special flags for particular modules
#	The standard suffix rule for compiling a Haskell file
#	adds these flags to the command line

absCSyn/AbsCSyn_HC_OPTS 	= -fno-omit-reexported-instances
basicTypes/IdInfo_HC_OPTS 	= -K2m
coreSyn/AnnCoreSyn_HC_OPTS 	= -fno-omit-reexported-instances
hsSyn/HsExpr_HC_OPTS 		= -K2m
hsSyn/HsSyn_HC_OPTS 		= -fno-omit-reexported-instances
main/Main_HC_OPTS 		= -fvia-C
main/CmdLineOpts_HC_OPTS 	= -fvia-C
nativeGen/PprMach_HC_OPTS 	= -K2m
nativeGen/MachMisc_HC_OPTS 	= -K2m -fvia-C
parser/UgenAll_HC_OPTS 		= -fvia-C '-\#include"hspincl.h"'
parser/UgenUtil_HC_OPTS 	= -fvia-C '-\#include"hspincl.h"'
parser/U_constr_HC_OPTS 	= -fvia-C '-\#include"hspincl.h"'
parser/U_binding_HC_OPTS 	= -fvia-C '-\#include"hspincl.h"'
parser/U_pbinding_HC_OPTS 	= -fvia-C '-\#include"hspincl.h"'
parser/U_entidt_HC_OPTS 	= -fvia-C '-\#include"hspincl.h"'
parser/U_list_HC_OPTS 		= -fvia-C '-\#include"hspincl.h"'
parser/U_literal_HC_OPTS 	= -fvia-C '-\#include"hspincl.h"'
parser/U_maybe_HC_OPTS 		= -fvia-C '-\#include"hspincl.h"'
parser/U_either_HC_OPTS 	= -fvia-C '-\#include"hspincl.h"'
parser/U_qid_HC_OPTS 		= -fvia-C '-\#include"hspincl.h"'
parser/U_tree_HC_OPTS 		= -fvia-C '-\#include"hspincl.h"'
parser/U_ttype_HC_OPTS 		= -fvia-C '-\#include"hspincl.h"'
prelude/PrimOp_HC_OPTS 		= -K3m
reader/Lex_HC_OPTS		= -K2m -H16m -fvia-C
ifeq "$(Ghc2_0)" "NO"
reader/Lex_HC_OPTS		+= -fomit-derived-read
endif
reader/ReadPrefix_HC_OPTS 	= -fvia-C '-\#include"hspincl.h"'
rename/ParseIface_HC_OPTS 	= -Onot -H16m
rename/ParseType_HC_OPTS 	= -Onot -H16m
rename/ParseUnfolding_HC_OPTS 	= -Onot -H16m
ifeq "$(TARGETPLATFORM)" "hppa1.1-hp-hpux9"
rename/RnMonad_HC_OPTS 		= -fvia-C -O2 -O2-for-C
else
rename/RnMonad_HC_OPTS 		= -fvia-C
endif
rename/RnEnv_HC_OPTS 		= -fvia-C
rename/RnSource_HC_OPTS		= -H12m
rename/RnIfaces_HC_OPTS		= -H8m -fvia-C
rename/RnNames_HC_OPTS		= -H12m
specialise/Specialise_HC_OPTS 	= -Onot -H12m
stgSyn/StgSyn_HC_OPTS 		= -fno-omit-reexported-instances
typecheck/TcGenDeriv_HC_OPTS	= -H10m
typecheck/TcExpr_HC_OPTS	= -H10m
typecheck/TcEnv_HC_OPTS		= -H10m
utils/Argv_HC_OPTS 		= -fvia-C
utils/SST_HC_OPTS 		= -fvia-C
utils/PrimPacked_HC_OPTS	= -fvia-C -monly-3-regs
utils/FastString_HC_OPTS	= -fvia-C
utils/StringBuffer_HC_OPTS	= -fvia-C
utils/Digraph_HC_OPTS		= -fglasgow-exts -fvia-C


# ----------------------------------------------------------------------------
#		C compilations

SRC_C_OPTS     += -O -Iparser -I. -IcodeGen


# ----------------------------------------------------------------------------
#		Parsers

# Main parser uses Yacc/Bison
SRC_YACC_OPTS  += -d -v

#
# Want to keep the intermediate (included in src distribs).
#
.PRECIOUS: %.tab.c %.tab.h parser/hslexer.c

parser/hschooks.o : parser/hschooks.c
	@$(RM) $@
	$(HC) -c -o $@ $(HCFLAGS) parser/hschooks.c


# Interface-file parser uses Happy
SRC_HAPPY_OPTS += +RTS -K2m -H10m -RTS

rename/ParseIface.hs : rename/ParseIface.y
	@$(RM) rename/ParseIface.hs rename/ParseIface.hinfo
	$(HAPPY) $(HAPPY_OPTS) -g rename/ParseIface.y
	@chmod 444 rename/ParseIface.hs

rename/ParseType.hs : rename/ParseType.y
	@$(RM) rename/ParseType.hs rename/ParseType.hinfo
	$(HAPPY) $(HAPPY_OPTS) -g rename/ParseType.y
	@chmod 444 rename/ParseType.hs

rename/ParseUnfolding.hs : rename/ParseUnfolding.y
	@$(RM) rename/ParseUnfolding.hs rename/ParseUnfolding.hinfo
	$(HAPPY) $(HAPPY_OPTS) -g rename/ParseUnfolding.y
	@chmod 444 rename/ParseUnfolding.hs

#----------------------------------------------------------------------
#
# Building the stand-alone parser
#
all :: hsp

hsp: parser/printtree.o parser/main.o libhsp.a 
	$(CC) -o $@ $(CC_OPTS) $^

CLEAN_FILES += hsp
#-----------------------------------------------------------------------------
# 		Interface files

# LoopHack.lhc is an SLPJ addition to fix a profiling problem.  See comments
# inside it. (compilation is handled by the suffix rules).

#
# Building the loop breakers from .lhi (0.2x) or .hi-boot (2.xx) files
#
ifeq ($(Ghc2_0),YES)

BOOT_HI_STEMS = $(basename $(wildcard */*.hi-boot))

hi-boot :: 
	@for n in $(BOOT_HI_STEMS); \
	do  \
	  echo Creating $$n.hi ; \
	  $(RM) $$n.hi ; \
	  $(CP) $$n.hi-boot $$n.hi || ( $(RM) $$n.hi && exit 1 ) ; \
	done 
#
# Have the collector *Loop.hs interface files be generated just after having
# created the .hi's from the .hi-boot's. 

hi-boot :: $(patsubst %.hs,%.hi,$(LOOPS_HS))

else
%.hi : %.lhi
	$(RM) $@
	$(UNLIT) $<  $@ || ( $(RM) $@ && exit 1 )
	@chmod 444 $@
endif

boot :: hi-boot

#-----------------------------------------------------------------------------
# 		Linking

SRC_LD_OPTS += -no-link-chk

# Build-specific
#SRC_LD_OPTS += "-pgml time gcc -B/projects/unsupported/gnu/sparc-sunos5/bin/g"


#-----------------------------------------------------------------------------
# 		install

# We don't want hsc treated as an ordinary executable,
# but put it together with the libraries.
# Also don't want any interface files intstalled

INSTALL_LIBEXECS += hsc hsp

#-----------------------------------------------------------------------------
# 		depend

# If we're using a 1.2 compiler to compile the compiler, need the old mkdepend stuff
#
# ToDo: define sep MKDEPENDHS_1.2 variable so that it can be readily overridden
# in a build.mk file.
#
ifeq ($(Ghc2_0),NO)
MKDEPENDHS      = $(MKDEPENDHS_1_2)
endif

#
# Before doing `make depend', need to build all derived Haskell source files
#
depend :: $(LOOPS) $(SRCS_UGNHS) rename/ParseIface.hs rename/ParseUnfolding.hs rename/ParseType.hs


ifeq "$(GhcWithHscBuiltViaC)" "YES"
SRC_MKDEPENDHS_OPTS += -o .hc
# When booting from .hc, (hackily) remove the suffix rule for 
# .l?hs -> .o, so that the .hc -> .o is used instead.
%.$(way_)o : %.lhs
%.$(way_)o : %.hs
else
  SRCS_MKDEPENDHS = $(SRCS_HC) # should add $(LOOPS) ?
endif


#-----------------------------------------------------------------------------
# 		clean

CLEAN_FILES += $(wildcard */*.$(way_)o */*.$(way_)hi) \
               $(SRCS_UGNC) $(SRCS_UGNH) \
               $(SRCS_UGNHS)\
	       parser/hslexer.c parser/hsparser.tab.h parser/hsparser.tab.c

# Extra tidy, remove the .hc files (if you've got them).
MAINTAINER_CLEAN_FILES += $(wildcard */*.hc)


#-----------------------------------------------------------------------------
# 		TAGS

SRC_HSTAGS_OPTS += -fglasgow-exts -cpp


#-----------------------------------------------------------------------------
# 		Include target-rule boilerplate

include $(TOP)/mk/target.mk

#
# Special extra dependencies for yukky ugen stuff
#

parser/%.o : parser/%.c $(SRCS_UGNH)
parser/hslexer.o : parser/hslexer.c parser/hsparser.tab.h

parser/hspincl.h : $(SRCS_UGNH)
parser/UgenAll.o : parser/hspincl.h
parser/UgenUtil.o : parser/hspincl.h
parser/U_constr.o : parser/hspincl.h
parser/U_binding.o : parser/hspincl.h
parser/U_pbinding.o : parser/hspincl.h
parser/U_entidt.o : parser/hspincl.h
parser/U_list.o : parser/hspincl.h
parser/U_literal.o : parser/hspincl.h
parser/U_maybe.o : parser/hspincl.h
parser/U_either.o : parser/hspincl.h
parser/U_qid.o : parser/hspincl.h
parser/U_tree.o : parser/hspincl.h
parser/U_ttype.o : parser/hspincl.h
parser/printtree.o : parser/hspincl.h
reader/ReadPrefix.o : parser/hspincl.h
