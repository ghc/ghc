# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.69 2000/01/25 10:15:55 simonmar Exp $

TOP = ..
include $(TOP)/mk/boilerplate.mk

#-----------------------------------------------------------------------------
# Building hsc different ways (default is just `normal' sequential)
WAYS=$(GhcCompilerWays)

# -----------------------------------------------------------------------------
#		Set HS_PROG, LIBRARY

# Note: there have been reports of people running up against the ARG_MAX limit
# when linking hsc with all its constituent object files. The likely source of 
# the problem is that the environment is a bit too big, so a workaround could
# be to do ` env PATH=$(PATH) make hsc 'to minimise the environment. (or the
# equivalent of `env' if it doesn't exist locally).
#
HS_PROG=hsc$(_way)


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
  profiling parser usageSP cprAnalysis


ifeq ($(GhcWithNativeCodeGen),YES)
DIRS += nativeGen
else
SRC_HC_OPTS += -DOMIT_NATIVE_CODEGEN
endif


HS_SRCS = $(foreach dir,$(DIRS),$(wildcard $(dir)/*.lhs))

ifneq "$(GhcWithHscBuiltViaC)" "YES"
HS_SRCS += rename/ParseIface.hs parser/Parser.hs
endif

HCS      = $(patsubst %.lhs, %.hc, $(patsubst %.hs, %.hc, $(HS_SRCS)))

# ParseIface.hs ain't part of HS_SRCS when this is on..
ifeq "$(GhcWithHscBuiltViaC)" "YES"
HCS      += rename/ParseIface.hc parser/Parser.hc
endif

HS_OBJS  = \
  $(patsubst %.hc, %.o, $(HCS)) \
  parser/ctypes.o parser/hschooks.o


DESTDIR       = $(INSTALL_LIBRARY_DIR_GHC)

#
# Add misc .c helper code (used by the frontend.)
#
C_SRCS += parser/ctypes.c parser/hschooks.c

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
# Compiler to use for building hsc, use the build tree
# driver when booting.
#
ifneq "$(GhcWithHscBuiltViaC)" "YES"
HC=$(WithGhcHc)
else
HC=$(GHC_INPLACE)
endif

# magic from GNU make manual to convert a list of values
# into a colon-separated list
empty:=
space:= $(empty) $(empty)

SRC_HC_OPTS += \
  -cpp -fglasgow-exts -Rghc-timing \
  -I. -IcodeGen -InativeGen -Iparser \
  -i$(subst $(space),:,$(DIRS))

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
main/Main_HC_OPTS 		= -fvia-C
main/Constants_HC_OPTS		= -DHscIfaceFileVersion=$(HscIfaceFileVersion)

ifneq "$(GhcWithHscBuiltViaC)" "YES"
ifeq "$(GhcReportCompiles)" "YES"
main/Main_HC_OPTS 		+= -syslib misc -DREPORT_TO_MOTHERLODE
endif
endif

main/CmdLineOpts_HC_OPTS 	= -fvia-C -K6m
nativeGen/PprMach_HC_OPTS 	= -K2m
nativeGen/MachMisc_HC_OPTS 	= -K2m -fvia-C
nativeGen/MachCode_HC_OPTS 	= -H10m

# Avoids Bug in 3.02, it seems
usageSP/UsageSPInf_HC_OPTS	= -Onot

prelude/PrimOp_HC_OPTS 		= -H12m -K3m
parser/Lex_HC_OPTS		= -K2m -H16m -fvia-C

rename/ParseIface_HC_OPTS 	+= -Onot -H45m -K2m -fno-warn-incomplete-patterns
rename/ParseIface_HAPPY_OPTS    += -g

parser/Parser_HC_OPTS 		+= -Onot -H80m -optCrts-M80m -K2m -fno-warn-incomplete-patterns
parser/Parser_HAPPY_OPTS    	+= -g

ifeq "$(TARGETPLATFORM)" "hppa1.1-hp-hpux9"
rename/RnMonad_HC_OPTS 		= -fvia-C -O2 -O2-for-C
endif

rename/RnEnv_HC_OPTS 		= -fvia-C
rename/RnSource_HC_OPTS		= -H12m
rename/RnIfaces_HC_OPTS		= -H8m -fvia-C
rename/RnExpr_HC_OPTS		= -H10m
rename/RnNames_HC_OPTS		= -H12m
rename/RnMonad_HC_OPTS		= -fvia-C
specialise/Specialise_HC_OPTS 	= -Onot -H12m
simplCore/Simplify_HC_OPTS	= -H15m	
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
utils/Argv_HC_OPTS 		= -fvia-C
utils/SST_HC_OPTS 		= -fvia-C
utils/FastString_HC_OPTS	= -fvia-C
utils/StringBuffer_HC_OPTS	= -fvia-C -fno-prune-tydecls
utils/Digraph_HC_OPTS		= -fglasgow-exts -fvia-C
utils/DirUtils_HC_OPTS 		= -fvia-C

ifeq "$(alpha_HOST_ARCH)" "1"
# "stbu" bug in alpha native code generator...
basicTypes/Unique_HC_OPTS	= -fvia-C
endif

# flags for PrimPacked:
#
# -monly-3-regs 
#	because it contains a 'ccall strlen', which gets inlined by
# 	gcc, causing a lack of registers.
#
# -optC-funfolding-interface-threshold10
#	To stop the definition of 'strLength', which calls strlen, getting
#	into the interface file and spreading the -monly-3-regs virus.
#	We need -optC here because the driver before 3.02 didn't understand
#	the -funfolding flags.

utils/PrimPacked_HC_OPTS	= -fvia-C -monly-3-regs -optC-funfolding-interface-threshold10

# Strictness analyser misbehaving in 2.10, fails to terminate on
# UpdAnal.lhs due to weird recursive datatype.  Bug was exposed by a
# separate bugfix to the fixpoint finder.
simplStg/UpdAnal_HC_OPTS	= -fno-strictness

# ----------------------------------------------------------------------------
#		C compilations

SRC_C_OPTS     += -O -I. -IcodeGen


# ----------------------------------------------------------------------------
#		Parsers/lexers

parser/hschooks.o : parser/hschooks.c
	@$(RM) $@
	$(HC) -c -o $@ $(HC_OPTS) parser/hschooks.c


# Interface-file parser uses Happy
SRC_HAPPY_OPTS += +RTS -K2m -H16m -RTS

rename/ParseIface.hs : rename/ParseIface.y
	@$(RM) rename/ParseIface.hs rename/ParseIface.hinfo
	$(HAPPY) $(HAPPY_OPTS) -g rename/ParseIface.y
	@chmod 444 rename/ParseIface.hs

parser/Parser.hs : parser/Parser.y
	@$(RM) parser/Parser.hs parser/ParseIface.hinfo
	$(HAPPY) $(HAPPY_OPTS) -g parser/Parser.y
	@chmod 444 parser/Parser.hs

#-----------------------------------------------------------------------------
# 		Linking

SRC_LD_OPTS += -no-link-chk

ifneq "$(GhcWithHscBuiltViaC)" "YES"
ifeq "$(GhcReportCompiles)" "YES"
SRC_LD_OPTS += -syslib misc -syslib exts
endif
endif

#-----------------------------------------------------------------------------
# 		install

# We don't want hsc treated as an ordinary executable,
# but put it together with the libraries.
# Also don't want any interface files intstalled

INSTALL_LIBEXECS += hsc

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

