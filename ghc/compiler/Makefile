# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.188 2001/08/15 18:31:57 qrczak Exp $

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
#
# The 'echo' commands simply spit the values of various make variables
# into Config.hs, whence they can be compiled and used by GHC itself

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
	@echo "cGhcWithNativeCodeGen = \"$(GhcWithNativeCodeGen)\"" >> $(CONFIG_HS)
	@echo "cGhcUnregisterised    = \"$(GhcUnregisterised)\"" >> $(CONFIG_HS)
	@echo "cLeadingUnderscore    = \"$(LeadingUnderscore)\"" >> $(CONFIG_HS)
	@echo "cRAWCPP_FLAGS         = \"$(RAWCPP_FLAGS)\"" >> $(CONFIG_HS)
	@echo "cGCC                  = \"$(WhatGccIsCalled)\"" >> $(CONFIG_HS)
	@echo "cMKDLL                = \"$(BLD_DLL)\"" >> $(CONFIG_HS)
	@echo "cGHC_DRIVER_DIR       = \"$(GHC_DRIVER_DIR)\"" >> $(CONFIG_HS)
	@echo "cGHC_TOUCHY           = \"$(GHC_TOUCHY)\"" >> $(CONFIG_HS)
	@echo "cGHC_TOUCHY_DIR       = \"$(GHC_TOUCHY_DIR)\"" >> $(CONFIG_HS)
	@echo "cGHC_UNLIT            = \"$(GHC_UNLIT)\"" >> $(CONFIG_HS)
	@echo "cGHC_UNLIT_DIR        = \"$(GHC_UNLIT_DIR)\"" >> $(CONFIG_HS)
	@echo "cGHC_MANGLER          = \"$(GHC_MANGLER)\"" >> $(CONFIG_HS)
	@echo "cGHC_MANGLER_DIR      = \"$(GHC_MANGLER_DIR)\"" >> $(CONFIG_HS)
	@echo "cGHC_SPLIT            = \"$(GHC_SPLIT)\"" >> $(CONFIG_HS)
	@echo "cGHC_SPLIT_DIR        = \"$(GHC_SPLIT_DIR)\"" >> $(CONFIG_HS)
	@echo "cGHC_SYSMAN           = \"$(GHC_SYSMAN)\"" >> $(CONFIG_HS)
	@echo "cGHC_SYSMAN_DIR       = \"$(GHC_SYSMAN_DIR)\"" >> $(CONFIG_HS)
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
endif

ifeq ($(GhcWithIlx),YES)
DIRS += ilxGen
SRC_HC_OPTS += -DILX
endif

#
# Canonicalize the GHC version number - assume it is has the form x.yy.[z] or 
# x.yy[.date] where date = yyyymmdd
#
# [First sed substitution gets rid of the '.', second gets rid of the date portion,
#  third appends a '0' if the z portion is missing]
#
CANON_HC_VERSION=$(shell echo "$(GhcVersion)" | sed -e 's/\.//g;s/^\(...\)........$$/\1/g;s/^\(...\)$$/\10/g')

# 20010625

ifeq "$(BootingFromHc)" "YES"
# HC files are always from a self-booted compiler
bootstrapped = YES
compiling_with_4xx=NO
else
bootstrapped = $(shell if (test $(CANON_HC_VERSION) -ge $(ProjectVersionInt)0); then echo YES; else echo NO; fi)
compiling_with_4xx = $(shell if (test $(CANON_HC_VERSION) -lt 5000); then echo YES; else echo NO; fi)
ghc_502_at_least = $(shell if (test $(CANON_HC_VERSION) -ge 5020); then echo YES; else echo NO; fi)
endif

# Only include GHCi if we're bootstrapping with at least version 411
ifeq "$(GhcWithInterpreter)" "YES"
ifeq "$(bootstrapped)" "YES"
SRC_HC_OPTS += -DGHCI
DIRS += ghci
endif
endif

HS_SRCS := $(foreach dir,$(DIRS),$(wildcard $(dir)/*.lhs) $(wildcard $(dir)/*.hs))
HS_SRCS := $(filter-out rename/ParseIface.hs parser/Parser.hs main/ParsePkgConf.hs $(CONFIG_HS), $(HS_SRCS))
HS_SRCS += $(CONFIG_HS)

# Enable code that assumes a MSDOSish subshell. See mk/config.mk.in
# for explanatory comment as to what this does.
ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
ghc_501_at_least = $(shell if (test $(CANON_HC_VERSION) -ge 5010); then echo YES; else echo NO; fi)

# -----------------------------------------------
# GHCi calls the C procedure 'rawSystem'; but alas GHC 4.08 
# does not have this.  Everything is fine if you are compiling
# GHC with GHC 5.02 or better, but lacking that we have the following
# hack: 
#	copy rawSystem.c from hslibs/lang/cbits
#	into main/ (where it'll be compiled and linked with the compiler)
#       (the Haskell-side code is ifdefed into main/SysTools.lhs)

ifneq "$(ghc_502_at_least)" "YES"
C_SRCS += main/rawSystem.c
SRC_CC_OPTS += -I$(GHC_LIB_DIR)/std/cbits
SRC_MKDEPENDC_OPTS += -I$(GHC_LIB_DIR)/std/cbits
HS_OBJS += main/rawSystem.o
main/rawSystem.c : $(FPTOOLS_TOP)/hslibs/lang/cbits/rawSystem.c
	$(CP) $< main
endif
endif
#		End of system hack
# -----------------------------------------------

ifneq "$(BootingFromHc)" "YES"
HS_SRCS += rename/ParseIface.hs parser/Parser.hs main/ParsePkgConf.hs
endif

HCS      = $(patsubst %.lhs, %.hc, $(patsubst %.hs, %.hc, $(HS_SRCS)))

ifeq "$(BootingFromHc)" "YES"
HCS	+= rename/ParseIface.hc parser/Parser.hc main/ParsePkgConf.hc
endif

#
# Add misc .c helper code (used by the frontend.)
#
C_SRCS += parser/hschooks.c

HS_OBJS = \
  $(patsubst %.hc, %.o, $(HCS)) \
  $(patsubst %.c, %.o, $(C_SRCS))

DESTDIR       = $(INSTALL_LIBRARY_DIR_GHC)

#
# Big Fudge to get around inherent problem that Makefile setup
# has got with 'mkdependC'.
# 
SRC_MKDEPENDC_OPTS += -D__GLASGOW_HASKELL__=$(ProjectVersionInt) -I$(GHC_INCLUDE_DIR)

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
ifeq "$(BootingFromHc)" "YES"
SRC_CC_OPTS += -D__GLASGOW_HASKELL__=$(ProjectVersionInt)
else
CC = $(HC)
CC_OPTS := $(addprefix -optc, $(CC_OPTS))
endif

# magic from GNU make manual to convert a list of values
# into a colon-separated list
empty:=
space:= $(empty) $(empty)

SRC_HC_OPTS += \
  -cpp -fglasgow-exts -Rghc-timing \
  -I. -IcodeGen -InativeGen -Iparser \
  -i$(subst $(space),:,$(DIRS)) 

# Omitted: 	-I$(GHC_INCLUDE_DIR)
# We should have -I$(GHC_INCLUDE_DIR) in SRC_HC_OPTS, 
# to avoid the use of an explicit path in GHC source files
#	(include "../includes/config.h"
# But alas GHC 4.08 (and others for all I know) uses this very
# same include path when compiling the .hc files it generates.
# Disaster!  Then the hc file sees the GHC 5.02 (or whatever)
# include files.   For the moment we've reverted to using
# an explicit path in the .hs sources

ifneq "$(mingw32_TARGET_OS)" "1"
SRC_HC_OPTS += -package concurrent -package posix -package text -package util
else
SRC_HC_OPTS += -package concurrent -package text -package util
endif

SRC_CC_OPTS += -Iparser -I. -I$(TOP)/includes -O
SRC_HC_OPTS += -recomp $(GhcHcOpts)
SRC_HC_OPTS += -H16M

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
ifeq "$(compiling_with_4xx)" "YES"
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
ghci/ByteCodeLink_HC_OPTS	= -fvia-C

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
ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
main/SysTools_HC_OPTS		+= '-\#include <windows.h>'
endif

# ----------------------------------------------------------------------------
#		C compilations

SRC_C_OPTS     += -O -I. -IcodeGen


# ----------------------------------------------------------------------------
#		Generate supporting stuff for prelude/PrimOp.lhs 
#		from prelude/primops.txt

CLEAN_FILES += prelude/primops.txt

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

ifneq "$(BootingFromHc)" "YES"
prelude/PrimOp.lhs prelude/PrimOp.o: $(PRIMOP_BITS)
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

ghc-inplace : $(HS_PROG)
	@$(RM) $@
	echo '#!/bin/sh' >>$@
	echo exec $(FPTOOLS_TOP_ABS_UNIX)/ghc/compiler/$(HS_PROG) -B$(FPTOOLS_TOP_ABS) '"$$@"' >>$@
	chmod 755 $@
ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
	@$(RM) $@.bat
	echo "@"$(subst /,\\,$(FPTOOLS_TOP_ABS)/ghc/compiler/$(HS_PROG)) "-B$(FPTOOLS_TOP_ABS) %1 %2 %3 %4 %5 %6 %7 %8 %9" >$@.bat
	chmod 755 $@.bat
endif

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
