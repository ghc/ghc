# -----------------------------------------------------------------------------
# Main ghc/compiler Makefile

# Targets:
#
#	all	builds stage1 compiler
#
#	boot stage=N   generate build dirs and dependencies for stage N.
#		       NB. Must be done before 'make stageN'.
#		       NB. Cannot 'make boot stage=2' until stage1 has
#		           been built (similarly for stage3).
#
#	stage1  (or stage=1) builds stage1 compiler
#	stage2  (or stage=2) builds stage2 compiler
#	stage3  (or stage=3) builds stage3 compiler
#

TOP = ..

# Use GHC for compiling C bits (NB. must be before boilerplate include)
#
UseGhcForCc = YES

include $(TOP)/mk/boilerplate.mk

#-----------------------------------------------------------------------------
# Counting source code lines

USER_SRCS = $(filter-out $(DERIVED_SRCS),$(SRCS))
count :
	./count_lines $(USER_SRCS)

#-----------------------------------------------------------------------------
# Building ghc different ways (default is just `normal' sequential)

WAYS=$(GhcCompilerWays)

# -----------------------------------------------------------------------------
# Bootstrapping

# The stage1/stage2/stage3 business is quite delicate.  Here's how it works:
# 
#  - the variable $(stage) holds the current stage number.  To build a 
#    particular stage, you say 'make stage=N' where N is 1, 2, or 3.
#    N defaults to 1.
#
#  - for stage N, object files and .hi files are placed inside 
#    the directory stageN, in subdirectories as per the sources.
#
#  - .hi-boot files are *linked* into the stageN tree, because in GHC 5.05+
#    the .hi-boot file must reside in the same place as the .hi file.
#
#  - we use explicit -o and -ohi options to direct the output from C & 
#    Haskell compilations.
#
#  - we generate a different .depend file for each build.  They need to be
#    different, because each stage might include different files: stage1
#    might not include GHCi, for example.  For each stage, a normal .depend
#    file is generated, and then post-processed to add the correct stageN/
#    prefix to each object and .hi filename.  The resulting .depend file
#    is named .depend-$(stage).  See the end of this Makefile for details.
#
#  - normal implicit rules don't work any more, because they're of the form
#
#        %.o : %.hs 
#
#    whereas we really need 
#
#	 stageN/%.o : %.hs
#
#    so suffix.mk now defines the appropriate suffix rules when
#    $(odir) is set to a non-empty value.  Here we set $(odir) to
#    stage1, stage2, or stage3.
#
#  There are other plausible designs that might work, but each has different
#  problems:
#
#  - using -odir and -hidir: GHC <= 4.08 doesn't support -hidir, and
#    anyway -odir puts all the objects in one directory (strips off the
#    subdirectory part), which eventually forces us to use VPATH to find
#    the sources.  I have a really bad feeling about VPATH.
#
#  - invoke make in the stageN subdirectory.  This probably requires VPATH
#    too.
#
#  - create a link tree.  The problem with requiring link trees is that 
#    Windows doesn't support symbolic links.

boot ::
	$(MKDIRHIER) stage$(stage)
	for i in $(ALL_DIRS); do \
	    $(MKDIRHIER) stage$(stage)/$$i; \
	done
# On Windows, we can't use symbolic links for the -hi-boot files
# because GHC itself is a Mingw program and does not understand
# symbolic links.  So we have to copy the files instead of link them.
# That means that if you modify a .hi-boot file in Windows, you
# have to to say 'make boot' again.
#
# PS: 'ln -s foo baz' takes 'foo' relative to the path to 'baz'
#     whereas 'cp foo baz' treats the two paths independently.
#     Hence the "../.." in the ln command line
ifeq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
	for i in */*hi-boot*; do \
	    cp -u -f $$i stage$(stage)/$$i; \
	done
else
	for i in */*hi-boot*; do \
	    $(LN_S) -f ../../$$i stage$(stage)/$$i || true ; \
	done
endif

ifeq "$(stage)" ""
stage=1
endif

ifeq "$(stage)" "1"
HC=$(GHC)
endif

ifeq "$(stage)" "2"
HC=$(GHC_STAGE1)
endif

ifeq "$(stage)" "3"
HC=$(GHC_STAGE2)
endif

stage1 ::
	$(MAKE) stage=1

stage2 ::
	$(MAKE) stage=2

stage3 ::
	$(MAKE) stage=3

odir=stage$(stage)

SRC_HC_OPTS += $(patsubst %, -i$(odir)/%, $(ALL_DIRS))

HS_OBJS = $(patsubst %, $(odir)/%, $(addsuffix .$(way_)o,$(basename $(HS_SRCS))))
C_OBJS = $(patsubst %, $(odir)/%, $(addsuffix .$(way_)o,$(basename $(C_SRCS))))

CLEAN_FILES += $(odir)/*/*.hi

ifeq "$(stage)" "1"
mostlyclean clean distclean maintainer-clean ::
	$(MAKE) $@ stage=2
	$(MAKE) $@ stage=3
endif

# -----------------------------------------------------------------------------
#		Set HS_PROG

# Note: there have been reports of people running up against the ARG_MAX limit
# when linking ghc with all its constituent object files. The likely source of 
# the problem is that the environment is a bit too big, so a workaround could
# be to do `env PATH=$(PATH) make ghc' to minimise the environment. (or the
# equivalent of `env' if it doesn't exist locally).
#
ifneq "$(way)" "dll"
ifneq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
HS_PROG=$(odir)/ghc$(_way)-$(ProjectVersion)
else
HS_PROG=$(odir)/ghc$(_way)
endif
else
HS_PROG=$(odir)/ghc-$(ProjectVersion)
endif

# -----------------------------------------------------------------------------
# Create compiler configuration
#
# The 'echo' commands simply spit the values of various make variables
# into Config.hs, whence they can be compiled and used by GHC itself

CONFIG_HS 	= main/Config.hs
boot :: $(CONFIG_HS)

$(CONFIG_HS) : $(FPTOOLS_TOP)/mk/config.mk Makefile
	@$(RM) -f $(CONFIG_HS)
	@echo "Creating $(CONFIG_HS) ... "
	@echo "module Config where" >>$(CONFIG_HS)
	@echo "cProjectName          = \"$(ProjectName)\"" >> $(CONFIG_HS)
	@echo "cProjectVersion       = \"$(ProjectVersion)\"" >> $(CONFIG_HS)
	@echo "cProjectVersionInt    = \"$(ProjectVersionInt)\"" >> $(CONFIG_HS)
	@echo "cProjectPatchLevel    = \"$(ProjectPatchLevel)\"" >> $(CONFIG_HS)
	@echo "cBooterVersion        = \"$(GhcVersion)\"" >> $(CONFIG_HS)
	@echo "cHscIfaceFileVersion  = \"$(HscIfaceFileVersion)\"" >> $(CONFIG_HS)
	@echo "cHOSTPLATFORM         = \"$(HOSTPLATFORM)\"" >> $(CONFIG_HS)
	@echo "cTARGETPLATFORM       = \"$(TARGETPLATFORM)\"" >> $(CONFIG_HS)
	@echo "cGhcWithNativeCodeGen = \"$(GhcWithNativeCodeGen)\"" >> $(CONFIG_HS)
	@echo "cGhcUnregisterised    = \"$(GhcUnregisterised)\"" >> $(CONFIG_HS)
	@echo "cLeadingUnderscore    = \"$(LeadingUnderscore)\"" >> $(CONFIG_HS)
	@echo "cRAWCPP_FLAGS         = \"$(RAWCPP_FLAGS)\"" >> $(CONFIG_HS)
	@echo "cGCC                  = \"$(WhatGccIsCalled)\"" >> $(CONFIG_HS)
	@echo "cMKDLL                = \"$(BLD_DLL)\"" >> $(CONFIG_HS)
	@echo "cPROJECT_DIR          = \"$(PROJECT_DIR)\"" >> $(CONFIG_HS)
	@echo "cGHC_DRIVER_DIR_REL   = \"$(GHC_DRIVER_DIR_REL)\"" >> $(CONFIG_HS)
	@echo "cGHC_TOUCHY_PGM       = \"$(GHC_TOUCHY_PGM)\"" >> $(CONFIG_HS)
	@echo "cGHC_TOUCHY_DIR_REL   = \"$(GHC_TOUCHY_DIR_REL)\"" >> $(CONFIG_HS)
	@echo "cGHC_UNLIT_PGM        = \"$(GHC_UNLIT_PGM)\"" >> $(CONFIG_HS)
	@echo "cGHC_UNLIT_DIR_REL    = \"$(GHC_UNLIT_DIR_REL)\"" >> $(CONFIG_HS)
	@echo "cGHC_MANGLER_PGM      = \"$(GHC_MANGLER_PGM)\"" >> $(CONFIG_HS)
	@echo "cGHC_MANGLER_DIR_REL  = \"$(GHC_MANGLER_DIR_REL)\"" >> $(CONFIG_HS)
	@echo "cGHC_SPLIT_PGM        = \"$(GHC_SPLIT_PGM)\"" >> $(CONFIG_HS)
	@echo "cGHC_SPLIT_DIR_REL    = \"$(GHC_SPLIT_DIR_REL)\"" >> $(CONFIG_HS)
	@echo "cGHC_SYSMAN_PGM       = \"$(GHC_SYSMAN)\"" >> $(CONFIG_HS)
	@echo "cGHC_SYSMAN_DIR_REL   = \"$(GHC_SYSMAN_DIR)\"" >> $(CONFIG_HS)
	@echo "cGHC_CP               = \"$(GHC_CP)\"" >> $(CONFIG_HS)
	@echo "cGHC_PERL             = \"$(GHC_PERL)\"" >> $(CONFIG_HS)
ifeq ($(GhcWithIlx),YES)
	@echo "cILX2IL               = \"$(ILX2IL)\"" >> $(CONFIG_HS)
	@echo "cILASM                = \"$(ILASM)\"" >> $(CONFIG_HS)
endif
	@echo "cEnableWin32DLLs      = \"$(EnableWin32DLLs)\"" >> $(CONFIG_HS)
	@echo "cCONTEXT_DIFF         = \"$(CONTEXT_DIFF)\"" >> $(CONFIG_HS)
	@echo "cHaveLibGmp           = \"$(HaveLibGmp)\"" >> $(CONFIG_HS)
	@echo "cUSER_WAY_NAMES       = \"$(USER_WAY_NAMES)\"" >> $(CONFIG_HS)
	@echo "cUSER_WAY_OPTS        = \"$(USER_WAY_OPTS)\"" >> $(CONFIG_HS)
	@echo "cDEFAULT_TMPDIR       = \"$(DEFAULT_TMPDIR)\"" >> $(CONFIG_HS)
	@echo done.

CLEAN_FILES += $(CONFIG_HS)

# -----------------------------------------------------------------------------
# Set SRCS etc.
#
# First figure out ALL_DIRS, the source sub-directories

ALL_DIRS = \
  utils basicTypes types hsSyn prelude rename typecheck deSugar coreSyn \
  specialise simplCore stranal stgSyn simplStg codeGen absCSyn main \
  profiling parser cprAnalysis compMan ndpFlatten cbits

# Make sure we include Config.hs even if it doesn't exist yet...
ALL_SRCS += $(CONFIG_HS)

# HsGeneric.hs is not used just now
EXCLUDED_SRCS += hsSyn/HsGeneric.hs

ifeq ($(GhcWithNativeCodeGen),YES)
ALL_DIRS += nativeGen
else
SRC_HC_OPTS += -DOMIT_NATIVE_CODEGEN
endif

ifeq ($(GhcWithIlx),YES)
ALL_DIRS += ilxGen
SRC_HC_OPTS += -DILX
endif

ifeq ($(GhcWithJavaGen),YES)
ALL_DIRS += javaGen
SRC_HC_OPTS += -DJAVA
endif

ifeq "$(BootingFromHc)" "YES"
# HC files are always from a self-booted compiler
bootstrapped = YES
compiling_with_4xx=NO
else
ifneq "$(findstring $(stage), 2 3)" ""
bootstrapped = YES
compiling_with_4xx = NO
else
bootstrapped = $(shell if (test $(GhcCanonVersion) -ge $(ProjectVersionInt) -a $(GhcPatchLevel) -ge $(ProjectPatchLevel)); then echo YES; else echo NO; fi)
compiling_with_4xx = $(shell if (test $(GhcCanonVersion) -lt 500); then echo YES; else echo NO; fi)
endif
endif

# Only include GHCi if we're bootstrapping with at least version 411
ifeq "$(GhcWithInterpreter) $(bootstrapped)" "YES YES"
# Yes, include the interepreter, readline, and Template Haskell extensions
SRC_HC_OPTS += -DGHCI -package haskell-src
ifneq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
SRC_HC_OPTS += -package unix
ifeq "$(GhcLibsWithReadline)" "YES"
SRC_HC_OPTS += -package readline 
endif
endif
ALL_DIRS += ghci
else
# No interpreter, so exclude Template Haskell modules
EXCLUDED_SRCS += deSugar/DsMeta.hs typecheck/TcSplice.lhs hsSyn/Convert.lhs
endif 

# There are some C files to include in HS_PROG, so add these to HS_OBJS
HS_OBJS	 += $(C_OBJS)

# -----------------------------------------------
# mkdependC stuff
#
# Big Fudge to get around inherent problem that Makefile setup
# has got with 'mkdependC'.
# 
SRC_MKDEPENDC_OPTS += -D__GLASGOW_HASKELL__=$(ProjectVersionInt)

# XXX not really correct, hschooks.c actually gets include files like
# RtsFlags.c from the installed GHC, but we can't tell mkdependC about that.
SRC_MKDEPENDC_OPTS += -I$(GHC_INCLUDE_DIR)

# -----------------------------------------------------------------------------
# 		Haskell compilations

SRC_HC_OPTS += \
  -cpp -fglasgow-exts -Rghc-timing \
  -I. -IcodeGen -InativeGen -Iparser

# Omitted: 	-I$(GHC_INCLUDE_DIR)
# We should have -I$(GHC_INCLUDE_DIR) in SRC_HC_OPTS, 
# to avoid the use of an explicit path in GHC source files
#	(include "../includes/config.h"
# But alas GHC 4.08 (and others for all I know) uses this very
# same include path when compiling the .hc files it generates.
# Disaster!  Then the hc file sees the GHC 5.02 (or whatever)
# include files.   For the moment we've reverted to using
# an explicit path in the .hs sources
#
# For the benefit of <5.00 compilers, do include GHC_INCLUDE_DIR
# when generating dependencies. (=> it gets passed onto mkdependHS,
# which needs it).
SRC_MKDEPENDHS_OPTS += -I$(GHC_INCLUDE_DIR)

# When bootstrapped, we don't make use of *any* packages
# (except possibly readline if GHCi is enabled, see above)
ifneq "$(bootstrapped)" "YES"
ifneq "$(mingw32_HOST_OS)" "1"
SRC_HC_OPTS += -package concurrent -package posix -package util
else
SRC_HC_OPTS += -package concurrent -package util
endif
endif

SRC_CC_OPTS += -Iparser -I. -O
SRC_HC_OPTS += -recomp $(GhcHcOpts) $(GhcStage$(stage)HcOpts)
SRC_HC_OPTS += -H16M

ifeq "$(BootingFromHc)" "YES"
SRC_CC_OPTS += -D__GLASGOW_HASKELL__=$(ProjectVersionInt)
endif

# 	Special flags for particular modules
#	The standard suffix rule for compiling a Haskell file
#	adds these flags to the command line

prelude/PrimOp_HC_OPTS 		= -no-recomp -H80m

# because the NCG can't handle the 64-bit math in here
prelude/PrelRules_HC_OPTS	= -fvia-C

main/ParsePkgConf_HC_OPTS	+= -fno-warn-incomplete-patterns
# Use -fvia-C since the NCG can't handle the narrow16Int# (and intToInt16#?)
# primops on all platforms.
parser/Parser_HC_OPTS 		+= -Onot -fno-warn-incomplete-patterns -fvia-C

# The latest GHC version doesn't have a -K option yet, and it doesn't
# seem to be necessary anymore for the modules below.
ifeq "$(compiling_with_4xx)" "YES"
parser/Parser_HC_OPTS 		+= -K2m
endif

ifeq "$(HOSTPLATFORM)" "hppa1.1-hp-hpux9"
rename/RnMonad_HC_OPTS 		=  -O2 -O2-for-C
endif

utils/Digraph_HC_OPTS		= -fglasgow-exts 

ifeq "$(bootstrapped)" "YES"
utils/Binary_HC_OPTS		= -funbox-strict-fields
endif

# 4.08.2's NCG can't cope with Binary
ifeq "$(compiling_with_4xx)" "YES"
utils/Binary_HC_OPTS		+= -fvia-C
endif

# ByteCodeItbls uses primops that the NCG doesn't support yet.
ghci/ByteCodeItbls_HC_OPTS	+= -fvia-C
ghci/ByteCodeLink_HC_OPTS	+= -fvia-C -monly-3-regs

# BinIface and Binary take ages to both compile and run if you don's use -O
main/BinIface_HC_OPTS		+= -O
utils/Binary_HC_OPTS		+= -O
utils/FastMutInt_HC_OPTS	+= -O


# CSE interacts badly with top-level IORefs (reportedly in DriverState and
# DriverMkDepend), causing some of them to be commoned up.  We have a fix for
# this in 5.00+, but earlier versions of the compiler will need CSE turned off.
# To be on the safe side, we disable CSE in *all* modules with top-level IORefs.
compMan/CompManager_HC_OPTS	= -fno-cse
ghci/InteractiveUI_HC_OPTS	= -fno-cse
main/CmdLineOpts_HC_OPTS	= -fno-cse
main/DriverFlags_HC_OPTS	= -fno-cse
main/DriverMkDepend_HC_OPTS	= -fno-cse
main/DriverPipeline_HC_OPTS	= -fno-cse
main/DriverState_HC_OPTS	= -fno-cse
main/DriverUtil_HC_OPTS		= -fno-cse
main/Finder_HC_OPTS		= -fno-cse
main/SysTools_HC_OPTS		= -fno-cse

# The #include is vital for the via-C route, else the C
# compiler doesn't realise that the stcall foreign imports are indeed
# stdcall, and doesn't generate the Foo@8 name for them
ifeq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
main/SysTools_HC_OPTS		+= '-\#include <windows.h>' '-\#include <process.h>'
endif

# Required due to use of Concurrent.myThreadId
utils/Panic_HC_OPTS += -fvia-C

parser/Lexer_HC_OPTS += -funbox-strict-fields

# ghc_strlen percolates through so many modules that it is easier to get its
# prototype via a global option instead of a myriad of per-file OPTIONS
SRC_HC_OPTS += '-\#include "hschooks.h"'

# ----------------------------------------------------------------------------
#		Generate supporting stuff for prelude/PrimOp.lhs 
#		from prelude/primops.txt

GENPOC=$(TOP)/utils/genprimopcode/genprimopcode

PRIMOP_BITS=primop-data-decl.hs-incl \
            primop-tag.hs-incl  \
            primop-list.hs-incl  \
            primop-has-side-effects.hs-incl  \
            primop-out-of-line.hs-incl  \
            primop-commutable.hs-incl  \
            primop-needs-wrapper.hs-incl  \
            primop-can-fail.hs-incl  \
            primop-strictness.hs-incl  \
            primop-primop-info.hs-incl

CLEAN_FILES += prelude/primops.txt
CLEAN_FILES += $(PRIMOP_BITS)

SRC_CPP_OPTS += -I$(GHC_INCLUDE_DIR)
SRC_CPP_OPTS += ${GhcCppOpts}

ifneq "$(BootingFromHc)" "YES"
prelude/PrimOp.lhs $(odir)/prelude/PrimOp.o: $(PRIMOP_BITS)
endif

ifneq "$(BootingFromHc)" "YES"
depend :: $(PRIMOP_BITS)
endif

primop-data-decl.hs-incl: prelude/primops.txt
	$(GENPOC) --data-decl          < $< > $@
primop-tag.hs-incl: prelude/primops.txt
	$(GENPOC) --primop-tag         < $< > $@
primop-list.hs-incl: prelude/primops.txt
	$(GENPOC) --primop-list        < $< > $@
primop-has-side-effects.hs-incl: prelude/primops.txt
	$(GENPOC) --has-side-effects   < $< > $@
primop-out-of-line.hs-incl: prelude/primops.txt
	$(GENPOC) --out-of-line        < $< > $@
primop-commutable.hs-incl: prelude/primops.txt
	$(GENPOC) --commutable         < $< > $@
primop-needs-wrapper.hs-incl: prelude/primops.txt
	$(GENPOC) --needs-wrapper      < $< > $@
primop-can-fail.hs-incl: prelude/primops.txt
	$(GENPOC) --can-fail           < $< > $@
primop-strictness.hs-incl: prelude/primops.txt
	$(GENPOC) --strictness         < $< > $@
primop-primop-info.hs-incl: prelude/primops.txt
	$(GENPOC) --primop-primop-info < $< > $@

# Usages aren't used any more; but the generator 
# can still generate them if we want them back
primop-usage.hs-incl: prelude/primops.txt
	$(GENPOC) --usage              < $< > $@


#-----------------------------------------------------------------------------
# 		Linking

SRC_LD_OPTS += -no-link-chk 

# -----------------------------------------------------------------------------
# create ghc-inplace, a convenient way to run ghc from the build tree...

all :: $(odir)/ghc-inplace ghc-inplace

$(odir)/ghc-inplace : $(HS_PROG)
	@$(RM) $@
	echo '#!/bin/sh' >>$@
	echo exec $(FPTOOLS_TOP_ABS)/ghc/compiler/$(HS_PROG) '-B$(subst \,\\,$(FPTOOLS_TOP_ABS_PLATFORM))' '"$$@"' >>$@
	chmod 755 $@

ghc-inplace : stage1/ghc-inplace
	$(LN_S) -f $< $@

CLEAN_FILES += $(odir)/ghc-inplace ghc-inplace

#-----------------------------------------------------------------------------
# 		install

# We don't want ghc treated as an ordinary executable,
# but put it together with the libraries.
# Also don't want any interface files installed

DESTDIR = $(INSTALL_LIBRARY_DIR_GHC)

ifneq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
INSTALL_LIBEXECS += $(HS_PROG)
else
INSTALL_PROGS += $(HS_PROG)
endif

# ----------------------------------------------------------------------------
# profiling.

# rename/RnBinds_HC_OPTS += -auto-all
# rename/RnEnv_HC_OPTS += -auto-all
# rename/RnExpr_HC_OPTS += -auto-all
# rename/RnHiFiles_HC_OPTS += -auto-all
# rename/RnHsSyn_HC_OPTS += -auto-all
# rename/Rename_HC_OPTS += -auto-all
# rename/RnIfaces_HC_OPTS += -auto-all
# rename/RnNames_HC_OPTS += -auto-all
# rename/RnSource_HC_OPTS += -auto-all
# rename/RnTypes_HC_OPTS += -auto-all
# 
# typecheck/Inst_HC_OPTS += -auto-all
# typecheck/TcBinds_HC_OPTS += -auto-all
# typecheck/TcClassDcl_HC_OPTS += -auto-all
# typecheck/TcDefaults_HC_OPTS += -auto-all
# typecheck/TcDeriv_HC_OPTS += -auto-all
# typecheck/TcEnv_HC_OPTS += -auto-all
# typecheck/TcExpr_HC_OPTS += -auto-all
# typecheck/TcForeign_HC_OPTS += -auto-all
# typecheck/TcGenDeriv_HC_OPTS += -auto-all
# typecheck/TcHsSyn_HC_OPTS += -auto-all
# typecheck/TcIfaceSig_HC_OPTS += -auto-all
# typecheck/TcInstDcls_HC_OPTS += -auto-all
# typecheck/TcMatches_HC_OPTS += -auto-all
# typecheck/TcMonoType_HC_OPTS += -auto-all
# typecheck/TcMType_HC_OPTS += -auto-all
# typecheck/TcPat_HC_OPTS += -auto-all
# typecheck/TcRnDriver_HC_OPTS += -auto-all
# #typecheck/TcRnMonad_HC_OPTS += -auto-all
# #typecheck/TcRnTypes_HC_OPTS += -auto-all
# typecheck/TcRules_HC_OPTS += -auto-all
# typecheck/TcSimplify_HC_OPTS += -auto-all
# typecheck/TcSplice_HC_OPTS += -auto-all
# typecheck/TcTyClsDecls_HC_OPTS += -auto-all
# typecheck/TcTyDecls_HC_OPTS += -auto-all
# typecheck/TcType_HC_OPTS += -auto-all
# typecheck/TcUnify_HC_OPTS += -auto-all
# 
# absCSyn/PprAbsC_HC_OPTS += -auto-all

coreSyn/CorePrep_HC_OPTS += -auto-all

#-----------------------------------------------------------------------------
# 		clean

MAINTAINER_CLEAN_FILES += parser/Parser.info main/ParsePkgConf.info

#-----------------------------------------------------------------------------
# 		Include target-rule boilerplate

# Don't use the default MKDEPENDHS stuff... we'll do our own, below
MKDEPENDHS_SRCS =
MKDEPENDC_SRCS =

include $(TOP)/mk/target.mk

# -----------------------------------------------------------------------------
# Dependencies

MKDEPENDHS_HC_OPTS = $(patsubst -i$(odir)/%, -i%, $(HC_OPTS))

MKDEPENDHS=$(HC)

# Must do this *after* including target.mk, because $(HS_SRCS) isn't set yet.
depend :: $(HS_SRCS) $(C_SRCS)
	touch .depend-BASE
ifneq "$(BootingFromHc)" "YES"
	$(MKDEPENDHS) -M -optdep-f -optdep.depend-BASE $(foreach way,$(WAYS),-optdep-s -optdep$(way)) $(foreach obj,$(MKDEPENDHS_OBJ_SUFFICES),-osuf $(obj)) $(MKDEPENDHS_OPTS) $(filter-out -split-objs, $(MKDEPENDHS_HC_OPTS)) $(HS_SRCS)
endif
	$(MKDEPENDC) -f .depend-BASE $(MKDEPENDC_OPTS) $(foreach way,$(WAYS),-s $(way)) -- $(CC_OPTS) -- $(C_SRCS) 
	$(PERL) -pe 'binmode(stdin); binmode(stdout); s@^(\S*\.o)@stage$(stage)/$$1@g; s@(\S*\.hi)@stage$(stage)/$$1@g' <.depend-BASE >.depend-$(stage)
# The binmode stuff tells perl not to add stupid ^M's to the output

-include .depend-$(stage)
