# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.146 2001/03/26 22:42:41 qrczak Exp $

TOP = ..
include $(TOP)/mk/boilerplate.mk

#-----------------------------------------------------------------------------
# Building ghc different ways (default is just `normal' sequential)
WAYS=$(GhcCompilerWays)

# -----------------------------------------------------------------------------
#		Set HS_PROG

# Note: there have been reports of people running up against the ARG_MAX limit
# when linking ghc with all its constituent object files. The likely source of 
# the problem is that the environment is a bit too big, so a workaround could
# be to do ` env PATH=$(PATH) make ghc 'to minimise the environment. (or the
# equivalent of `env' if it doesn't exist locally).
#
ifneq "$(way)" "dll"
HS_PROG=ghc$(_way)-$(ProjectVersion)
else
HS_PROG=ghc-$(ProjectVersion)
endif

# -----------------------------------------------------------------------------
# Create compiler configuration

CURRENT_DIR	= ghc/compiler
CONFIG_HS 	= main/Config.hs
boot :: $(CONFIG_HS)

$(CONFIG_HS) : $(FPTOOLS_TOP)/mk/config.mk Makefile
	@$(RM) -f $(CONFIG_HS)
	@echo -n "Creating $(CONFIG_HS) ... "
	@echo "module Config where" >>$(CONFIG_HS)
	@echo "cProjectName          = \"$(ProjectName)\"" >> $(CONFIG_HS)
	@echo "cProjectVersion       = \"$(ProjectVersion)\"" >> $(CONFIG_HS)
	@echo "cProjectVersionInt    = \"$(ProjectVersionInt)\"" >> $(CONFIG_HS)
	@echo "cBooterVersion        = \"$(GhcVersion)\"" >> $(CONFIG_HS)
	@echo "cHscIfaceFileVersion  = \"$(HscIfaceFileVersion)\"" >> $(CONFIG_HS)
	@echo "cHOSTPLATFORM         = \"$(HOSTPLATFORM)\"" >> $(CONFIG_HS)
	@echo "cTARGETPLATFORM       = \"$(TARGETPLATFORM)\"" >> $(CONFIG_HS)
	@echo "cCURRENT_DIR          = \"$(CURRENT_DIR)\"" >> $(CONFIG_HS)
	@echo "cGHC_LIB_DIR          = \"$(GHC_LIB_DIR)\"" >> $(CONFIG_HS)
	@echo "cGHC_RUNTIME_DIR      = \"$(GHC_RUNTIME_DIR)\"" >> $(CONFIG_HS)
	@echo "cGHC_UTILS_DIR        = \"$(GHC_UTILS_DIR)\"" >> $(CONFIG_HS)
	@echo "cGHC_INCLUDE_DIR      = \"$(GHC_INCLUDE_DIR)\"" >> $(CONFIG_HS)
	@echo "cGHC_DRIVER_DIR       = \"$(GHC_DRIVER_DIR)\"" >> $(CONFIG_HS)
	@echo "cGCC                  = \"$(WhatGccIsCalled)\"" >> $(CONFIG_HS)
	@echo "cMkDLL                = \"$(BLD_DLL)\"" >> $(CONFIG_HS)
	@echo "cGhcWithNativeCodeGen = \"$(GhcWithNativeCodeGen)\"" >> $(CONFIG_HS)
	@echo "cGhcUnregisterised    = \"$(GhcUnregisterised)\"" >> $(CONFIG_HS)
	@echo "cLeadingUnderscore    = \"$(LeadingUnderscore)\"" >> $(CONFIG_HS)
	@echo "cGHC_UNLIT            = \"$(GHC_UNLIT)\"" >> $(CONFIG_HS)
	@echo "cGHC_MANGLER          = \"$(GHC_MANGLER)\"" >> $(CONFIG_HS)
	@echo "cGHC_SPLIT            = \"$(GHC_SPLIT)\"" >> $(CONFIG_HS)
	@echo "cGHC_SYSMAN           = \"$(GHC_SYSMAN)\"" >> $(CONFIG_HS)
	@echo "cEnableWin32DLLs      = \"$(EnableWin32DLLs)\"" >> $(CONFIG_HS)
	@echo "cCP                   = \"$(CP)\"" >> $(CONFIG_HS)
	@echo "cRM                   = \"$(RM)\"" >> $(CONFIG_HS)
	@echo "cCONTEXT_DIFF         = \"$(CONTEXT_DIFF)\"" >> $(CONFIG_HS)
	@echo "cHaveLibGmp           = \"$(HaveLibGmp)\"" >> $(CONFIG_HS)
	@echo "cUSER_WAY_NAMES       = \"$(USER_WAY_NAMES)\"" >> $(CONFIG_HS)
	@echo "cUSER_WAY_OPTS        = \"$(USER_WAY_OPTS)\"" >> $(CONFIG_HS)
	@echo "clibdir               = \"$(libdir)\"" >> $(CONFIG_HS)
	@echo "clibexecdir           = \"$(libexecdir)\"" >> $(CONFIG_HS)
	@echo "cdatadir              = \"$(datadir)\"" >> $(CONFIG_HS)
	@echo "cbindir               = \"$(bindir)\"" >> $(CONFIG_HS)
	@echo "cDEFAULT_TMPDIR       = \"$(DEFAULT_TMPDIR)\"" >> $(CONFIG_HS)
	@echo "cFPTOOLS_TOP_ABS      = \"$(FPTOOLS_TOP_ABS)\"" >> $(CONFIG_HS)
	@echo "cRAWCPP               = \"$(RAWCPP)\"" >> $(CONFIG_HS)
	@echo done.

CLEAN_FILES += $(CONFIG_HS)

# -----------------------------------------------------------------------------
# 		Set SRCS, HCS, OBJS
#
# First figure out DIRS, the source sub-directories
# Then derive SRCS by looking in them
#

DIRS = \
  utils basicTypes types hsSyn prelude rename typecheck deSugar coreSyn \
  specialise simplCore stranal stgSyn simplStg codeGen absCSyn main \
  profiling parser usageSP cprAnalysis javaGen compMan

ifeq ($(GhcWithNativeCodeGen),YES)
DIRS += nativeGen
else
SRC_HC_OPTS += -DOMIT_NATIVE_CODEGEN
ifeq ($(GhcWithIlx),YES)
DIRS += ilxGen
SRC_HC_OPTS += -DILX
endif
endif

ghc_411_at_least = $(shell expr "$(GhcMinVersion)" \>= 11)

# Only include GHCi if we're bootstrapping with at least version 411
ifeq "$(GhcWithInterpreter)" "YES"
ifeq "$(ghc_411_at_least)" "1"
SRC_HC_OPTS += -DGHCI
DIRS += ghci
endif
endif

HS_SRCS := $(foreach dir,$(DIRS),$(wildcard $(dir)/*.lhs) $(wildcard $(dir)/*.hs))
HS_SRCS := $(filter-out rename/ParseIface.hs parser/Parser.hs main/ParsePkgConf.hs $(CONFIG_HS), $(HS_SRCS))
HS_SRCS += $(CONFIG_HS)

ifneq "$(BootingFromHc)" "YES"
HS_SRCS += rename/ParseIface.hs parser/Parser.hs main/ParsePkgConf.hs
endif

HCS      = $(patsubst %.lhs, %.hc, $(patsubst %.hs, %.hc, $(HS_SRCS)))

ifeq "$(BootingFromHc)" "YES"
HCS	+= rename/ParseIface.hc parser/Parser.hc main/ParsePkgConf.hs
endif

HS_OBJS  = \
  $(patsubst %.hc, %.o, $(HCS)) \
  parser/hschooks.o

DESTDIR       = $(INSTALL_LIBRARY_DIR_GHC)

#
# Add misc .c helper code (used by the frontend.)
#
C_SRCS += parser/hschooks.c

#
# Big Fudge to get around inherent problem that Makefile setup
# has got with 'mkdependC'.
# 
SRC_MKDEPENDC_OPTS += -D__GLASGOW_HASKELL__=$(ProjectVersionInt)

# -----------------------------------------------------------------------------
# 		Haskell compilations

#
# Compiler to use for building ghc, use the build tree
# driver when booting.
#
HC=$(GHC)

#
# Use GHC for compiling C bits
#
CC = $(HC)
CC_OPTS := $(addprefix -optc, $(CC_OPTS))

# magic from GNU make manual to convert a list of values
# into a colon-separated list
empty:=
space:= $(empty) $(empty)

SRC_HC_OPTS += \
  -cpp -fglasgow-exts -Rghc-timing \
  -I. -IcodeGen -InativeGen -Iparser \
  -i$(subst $(space),:,$(DIRS)) 

ifneq "$(mingw32_TARGET_OS)" "1"
SRC_HC_OPTS += -package concurrent -package posix -package text -package util
else
SRC_HC_OPTS += -package concurrent -package text -package util
endif

SRC_CC_OPTS += -Iparser -I. -I$(TOP)/includes -O
SRC_HC_OPTS += -recomp $(GhcHcOpts)
SRC_HC_OPTS += -H16m

# 	Special flags for particular modules
#	The standard suffix rule for compiling a Haskell file
#	adds these flags to the command line

# not sure if this is required --SDM
main/Main_HC_OPTS 		= -fvia-C

prelude/PrimOp_HC_OPTS 		= -no-recomp -H80m

# because the NCG can't handle the 64-bit math in here
prelude/PrelRules_HC_OPTS	= -fvia-C

rename/ParseIface_HC_OPTS 	+= -Onot -H45m -fno-warn-incomplete-patterns
parser/Parser_HC_OPTS 		+= -Onot -fno-warn-incomplete-patterns
main/ParsePkgConf_HC_OPTS	+= -fno-warn-incomplete-patterns

# The latest GHC version doesn't have a -K option yet, and it doesn't
# seem to be necessary anymore for the modules below.
ifeq "$(ghc_411_at_least)" "0"
rename/ParseIface_HC_OPTS 	+= -K2m
parser/Parser_HC_OPTS 		+= -K2m
endif

ifeq "$(TARGETPLATFORM)" "hppa1.1-hp-hpux9"
rename/RnMonad_HC_OPTS 		=  -O2 -O2-for-C
endif

utils/StringBuffer_HC_OPTS	= -fvia-C -fno-prune-tydecls
utils/Digraph_HC_OPTS		= -fglasgow-exts 

# flags for PrimPacked:
#
# -monly-3-regs 
#	because it contains a 'ccall strlen', which gets inlined by
# 	gcc, causing a lack of registers.
#
utils/PrimPacked_HC_OPTS	= -fvia-C -monly-3-regs

# ByteCodeItbls uses primops that the NCG doesn't support yet.
ghci/ByteCodeItbls_HC_OPTS	= -fvia-C

# CSE interacts badly with the top-level IORefs in DriverState, causing some
# of them to be commoned up.  We have a fix for this in 5.00+, but earlier
# versions of the compiler will need CSE turned off on this module.
main/DriverState_HC_OPTS	= -fno-cse

# ----------------------------------------------------------------------------
#		C compilations

SRC_C_OPTS     += -O -I. -IcodeGen


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
            primop-usage.hs-incl  \
            primop-primop-info.hs-incl

prelude/PrimOp.lhs prelude/PrimOp.o: $(PRIMOP_BITS)

depend :: $(PRIMOP_BITS)

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
primop-usage.hs-incl: prelude/primops.txt
	$(GENPOC) --usage              < $< > $@
primop-primop-info.hs-incl: prelude/primops.txt
	$(GENPOC) --primop-primop-info < $< > $@



# ----------------------------------------------------------------------------
#		Parsers/lexers

SRC_HAPPY_OPTS += +RTS -K2m -H16m -RTS  $(GHC_HAPPY_OPTS)

#-----------------------------------------------------------------------------
# 		Linking

SRC_LD_OPTS += -no-link-chk 

# -----------------------------------------------------------------------------
# create ghc-inplace, a convenient way to run ghc from the build tree...

all :: ghc-inplace

ghc-inplace : ghc
	@$(RM) $@
	echo '#!/bin/sh' >>$@
	echo exec $(FPTOOLS_TOP_ABS)/ghc/compiler/ghc -B$(FPTOOLS_TOP_ABS) '"$$@"' >>$@
	chmod 755 $@

CLEAN_FILES += ghc-inplace

#-----------------------------------------------------------------------------
# 		install

# We don't want ghc treated as an ordinary executable,
# but put it together with the libraries.
# Also don't want any interface files intstalled

INSTALL_LIBEXECS += $(HS_PROG)

#-----------------------------------------------------------------------------
# 		clean

CLEAN_FILES += $(wildcard */*.$(way_)o */*.$(way_)hi)
CLEAN_FILES += $(PRIMOP_BITS)

# Extra tidy, remove the .hc files (if you've got them).
MAINTAINER_CLEAN_FILES += $(wildcard */*.hc) \
	parser/Parser.info rename/ParseIface.info main/ParsePkgConf.info \
	parser/Parser.hs rename/ParseIface.hs main/ParsePkgConf.hs

#-----------------------------------------------------------------------------
# 		Include target-rule boilerplate

include $(TOP)/mk/target.mk
