# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.107 2000/11/03 11:30:23 sewardj Exp $

TOP = ..
include $(TOP)/mk/boilerplate.mk

#-----------------------------------------------------------------------------
# Building ghc different ways (default is just `normal' sequential)
WAYS=$(GhcCompilerWays)

# -----------------------------------------------------------------------------
#		Set HS_PROG, LIBRARY

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

LINK = ghc

# -----------------------------------------------------------------------------
# Create compiler configuration

CURRENT_DIR=ghc/compiler
CONFIG_HS = main/Config.hs
boot :: $(CONFIG_HS)

$(CONFIG_HS) : $(FPTOOLS_TOP)/mk/config.mk Makefile
	@$(RM) -f $(CONFIG_HS)
	@echo -n "Creating $(CONFIG_HS) ... "
	@echo "module Config where" >>$(CONFIG_HS)
	@echo "cProjectName          = \"$(ProjectName)\"" >> $(CONFIG_HS)
	@echo "cProjectVersion       = \"$(ProjectVersion)\"" >> $(CONFIG_HS)
	@echo "cProjectVersionInt    = \"$(ProjectVersionInt)\"" >> $(CONFIG_HS)
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
	@echo "cGhcWithNativeCodeGen = \"$(GhcWithNativeCodeGen)\"" >> $(CONFIG_HS)
	@echo "cLeadingUnderscore    = \"$(LeadingUnderscore)\"" >> $(CONFIG_HS)
	@echo "cGHC_UNLIT            = \"$(GHC_UNLIT)\"" >> $(CONFIG_HS)
	@echo "cGHC_MANGLER          = \"$(GHC_MANGLER)\"" >> $(CONFIG_HS)
	@echo "cGHC_SPLIT            = \"$(GHC_SPLIT)\"" >> $(CONFIG_HS)
	@echo "cGHC_STATS            = \"$(GHC_STATS)\"" >> $(CONFIG_HS)
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
#		Compilation history for Patrick

# Make the sources first, because that's what the compilation history needs
$(HS_PROG) :: $(HS_SRCS)


# -----------------------------------------------------------------------------
# 		Set SRCS, LOOPS, HCS, OBJS
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

# Only include GHCi if we're bootstrapping with at least version 409
ifeq "$(GhcWithInterpreter)" "YES"
ghc_409_at_least = $(shell expr "$(GhcMinVersion)" \>= 9)
ifeq "$(ghc_409_at_least)" "1"
SRC_HC_OPTS += -DGHCI
DIRS += ghci
endif
endif

HS_SRCS = $(foreach dir,$(DIRS),$(wildcard $(dir)/*.lhs) $(wildcard $(dir)/*.hs))

# ToDo: not clear why this is here -- messes up linking
#ifneq "$(GhcWithHscBuiltViaC)" "YES"
#HS_SRCS += rename/ParseIface.hs parser/Parser.hs
#endif

HCS      = $(patsubst %.lhs, %.hc, $(patsubst %.hs, %.hc, $(HS_SRCS)))

# ParseIface.hs ain't part of HS_SRCS when this is on..
ifeq "$(GhcWithHscBuiltViaC)" "YES"
HCS      += rename/ParseIface.hc parser/Parser.hc
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

# stuff you get for free in a source distribution
# 
# SRC_DIST_FILES += rename/ParseIface.hs rename/Parser.hs

# -----------------------------------------------------------------------------
# 		Haskell compilations

#
# Compiler to use for building ghc, use the build tree
# driver when booting.
#
ifneq "$(GhcWithHscBuiltViaC)" "YES"
HC=$(GHC)
else
HC=$(GHC_INPLACE)
endif

# magic from GNU make manual to convert a list of values
# into a colon-separated list
empty:=
space:= $(empty) $(empty)

SRC_HC_OPTS += \
  -cpp -fglasgow-exts \
  -Rghc-timing -I. -IcodeGen -InativeGen -Iparser \
  -i$(subst $(space),:,$(DIRS)) 

ghc_407_at_least = $(shell expr "$(GhcMinVersion)" \>= 7)
ifeq "$(ghc_407_at_least)" "1"
ifneq "$(mingw32_TARGET_OS)" "1"
SRC_HC_OPTS += -package concurrent -package posix -package text
else
SRC_HC_OPTS += -package concurrent -package text
endif
else
SRC_HC_OPTS += -syslib concurrent -syslib posix -syslib misc
endif

SRC_CC_OPTS += -Iparser -I. -I$(TOP)/includes -O

SRC_HC_OPTS += -recomp $(GhcHcOpts)

# 	Special flags for particular modules
#	The standard suffix rule for compiling a Haskell file
#	adds these flags to the command line

# Was 6m with 2.10
absCSyn/PprAbsC_HC_OPTS 	= -H10m

basicTypes/IdInfo_HC_OPTS 	= -K2m
codeGen/CgCase_HC_OPTS		= -fno-prune-tydecls
hsSyn/HsExpr_HC_OPTS 		= -K2m
main/Main_HC_OPTS 		= 

ifneq "$(GhcWithHscBuiltViaC)" "YES"
ifeq "$(GhcReportCompiles)" "YES"
main/Main_HC_OPTS 		+= -syslib misc -DREPORT_TO_MOTHERLODE
endif
endif

main/CmdLineOpts_HC_OPTS 	= -K6m
nativeGen/PprMach_HC_OPTS 	= -K2m
nativeGen/MachMisc_HC_OPTS 	= -K2m 
nativeGen/MachCode_HC_OPTS 	= -H10m

# Avoids Bug in 3.02, it seems
usageSP/UsageSPInf_HC_OPTS	= -Onot

prelude/PrimOp_HC_OPTS 		= -H12m -K3m

# because the NCG can't handle the 64-bit math in here
prelude/PrelRules_HC_OPTS	= -fvia-C

parser/Lex_HC_OPTS		= -K2m -H16m 
parser/Ctype_HC_OPTS		= -K2m

rename/ParseIface_HC_OPTS 	+= -Onot -H45m -K2m -fno-warn-incomplete-patterns

parser/Parser_HC_OPTS 		+= -Onot -H80m -optCrts-M80m -K2m -fno-warn-incomplete-patterns

ifeq "$(TARGETPLATFORM)" "hppa1.1-hp-hpux9"
rename/RnMonad_HC_OPTS 		=  -O2 -O2-for-C
endif

rename/RnEnv_HC_OPTS 		= 
rename/RnSource_HC_OPTS		= -H12m
rename/RnIfaces_HC_OPTS		= -H8m 
rename/RnExpr_HC_OPTS		= -H10m
rename/RnNames_HC_OPTS		= -H12m
rename/RnMonad_HC_OPTS		= 
specialise/Specialise_HC_OPTS 	= -Onot -H12m
simplCore/Simplify_HC_OPTS	= -H15m	
simplCore/OccurAnal_HC_OPTS	= -H10m
typecheck/TcGenDeriv_HC_OPTS	= -H10m

# tmp, -- SDM
specialise/Specialise_HC_OPTS 	+= -fno-prune-tydecls
hsSyn/HsBasic_HC_OPTS		+= -fno-prune-tydecls
simplCore/SimplMonad_HC_OPTS	+= -fno-prune-tydecls
absCSyn/AbsCSyn_HC_OPTS		+= -fno-prune-tydecls
absCSyn/AbsCUtils_HC_OPTS	+= -fno-prune-tydecls
codeGen/CgBindery_HC_OPTS	+= -fno-prune-tydecls
codeGen/CgLetNoEscape_HC_OPTS	+= -fno-prune-tydecls
codeGen/CgExpr_HC_OPTS		+= -fno-prune-tydecls
nativeGen/AbsCStixGen_HC_OPTS	+= -fno-prune-tydecls

# Was 10m for 2.10
typecheck/TcHsSyn_HC_OPTS	= -H15m	


# Was 10m for 2.10
typecheck/TcExpr_HC_OPTS	= -H15m
typecheck/TcBinds_HC_OPTS       = -H10m

typecheck/TcEnv_HC_OPTS		= -H10m
utils/SST_HC_OPTS 		= 
utils/StringBuffer_HC_OPTS	= -fvia-C -fno-prune-tydecls
utils/Digraph_HC_OPTS		= -fglasgow-exts 
utils/DirUtils_HC_OPTS 		= 

# temporary, due to bug in NCG (x86 spiller fails because a basic block
# contains both multiply and divide insns, probably soaking up all spill
# regs) (SDM, 1/6/2000), augmented by JRS 27 Oct 00.
utils/FastString_HC_OPTS	= -fvia-C

ifeq "$(alpha_HOST_ARCH)" "1"
# "stbu" bug in alpha native code generator...
basicTypes/Unique_HC_OPTS	= 
endif

# flags for PrimPacked:
#
# -monly-3-regs 
#	because it contains a 'ccall strlen', which gets inlined by
# 	gcc, causing a lack of registers.
#
# -optC-funfolding-interface-threshold7
#	To stop the definition of 'strLength', which calls strlen, getting
#	into the interface file and spreading the -monly-3-regs virus.
#	We need -optC here because the driver before 3.02 didn't understand
#	the -funfolding flags.

utils/PrimPacked_HC_OPTS	= -fvia-C -monly-3-regs -optC-funfolding-interface-threshold7

# Strictness analyser misbehaving in 2.10, fails to terminate on
# UpdAnal.lhs due to weird recursive datatype.  Bug was exposed by a
# separate bugfix to the fixpoint finder.
simplStg/UpdAnal_HC_OPTS	= -fno-strictness

ghci/StgInterp_HC_OPTS		= -fvia-C

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

prelude/PrimOp.o: $(PRIMOP_BITS)

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

parser/hschooks.o : parser/hschooks.c
	@$(RM) $@
	$(HC) -c -o $@ $(HC_OPTS) parser/hschooks.c


# Interface-file parser uses Happy
SRC_HAPPY_OPTS += +RTS -K2m -H16m -RTS  $(GHC_HAPPY_OPTS)

rename/ParseIface.hs : rename/ParseIface.y
	@$(RM) rename/ParseIface.hs rename/ParseIface.hinfo
	$(HAPPY) $(HAPPY_OPTS) rename/ParseIface.y
	@chmod 444 rename/ParseIface.hs

parser/Parser.hs : parser/Parser.y
	@$(RM) parser/Parser.hs parser/ParseIface.hinfo
	$(HAPPY) $(HAPPY_OPTS) parser/Parser.y
	@chmod 444 parser/Parser.hs

#-----------------------------------------------------------------------------
# 		Linking

SRC_LD_OPTS += -no-link-chk 
# REMOVED SLPJ
# -ldl

ifneq "$(GhcWithHscBuiltViaC)" "YES"
ifeq "$(GhcReportCompiles)" "YES"
SRC_LD_OPTS += -syslib misc -syslib exts
endif
endif

# -----------------------------------------------------------------------------
# create ghc-inplace, a convenient way to run ghc from the build tree...

all :: ghc-inplace

ghc-inplace : ghc
	@$(RM) $@
	echo '#!/bin/sh' >>$@
	echo exec $(FPTOOLS_TOP_ABS)/ghc/compiler/ghc -B$(FPTOOLS_TOP_ABS) '"$$@"' >>$@
	chmod 755 $@

CLEAN_FILES += ghc-inplace

# -----------------------------------------------------------------------------
# Create link to from ghc-x.xx to ghc...

all :: $(LINK)

$(LINK) : $(HS_PROG)
	@if ( $(PERL) -e '$$fn="$(LINK)"; exit ((! -f $$fn || -l $$fn) ? 0 : 1);' ); then \
	   echo "Creating a symbolic link from $(HS_PROG) to $(LINK)"; \
	   $(RM) $(LINK); \
	   $(LN_S) $(HS_PROG) $(LINK); \
	 else \
	   echo "Creating a symbolic link from $(HS_PROG) to $(LINK) failed: \`$(LINK)' already exists"; \
	   echo "Perhaps remove \`$(LINK)' manually?"; \
	   exit 1; \
	 fi;

CLEAN_FILES += $(LINK)

install ::
	@if ( $(PERL) -e '$$fn="$(bindir)/$(LINK)"; exit ((! -f $$fn || -l $$fn) ? 0 : 1);' ); then \
	   echo "Creating a symbol link from $(HS_PROG) to $(LINK) in $(bindir)"; \
	   $(RM) $(bindir)/$(LINK); \
	   $(LN_S) $(HS_PROG) $(bindir)/$(LINK); \
	 else \
	   echo "Creating a symbol link from $(HS_PROG) to $(LINK) in $(bindir) failed: \`$(bindir)/$(LINK)' already exists"; \
	   echo "Perhaps remove \`$(bindir)/$(LINK)' manually?"; \
	   exit 1; \
	 fi;

#-----------------------------------------------------------------------------
# 		install

# We don't want ghc treated as an ordinary executable,
# but put it together with the libraries.
# Also don't want any interface files intstalled

INSTALL_PROGS += $(HS_PROG)

#-----------------------------------------------------------------------------
# 		depend

#
# Before doing `make depend', need to build all derived Haskell source files
#
depend :: $(LOOPS)

ifneq "$(GhcWithHscBuiltViaC)" "YES"
depend :: rename/ParseIface.hs
endif

#-----------------------------------------------------------------------------
# 		clean

CLEAN_FILES += $(wildcard */*.$(way_)o */*.$(way_)hi)
CLEAN_FILES += $(PRIMOP_BITS)

# Extra tidy, remove the .hc files (if you've got them).
MAINTAINER_CLEAN_FILES += $(wildcard */*.hc) \
	parser/Parser.hs rename/ParseIface.hs


#-----------------------------------------------------------------------------
# 		TAGS setup

SRC_HSTAGS_OPTS += -fglasgow-exts -cpp
TAGS_HS_SRCS=$(foreach dir,$(DIRS),$(wildcard $(dir)/*.lhs))
TAGS_C_SRCS=$(strip $(foreach f,$(foreach dir,$(DIRS),$(wildcard $(dir)/*.c)),$(shell if [ -w $(f) ]; then echo $(f); else echo ''; fi)))

#-----------------------------------------------------------------------------
# 		Include target-rule boilerplate

include $(TOP)/mk/target.mk

parser/%.o : parser/%.c

