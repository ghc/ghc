#################################################################################
#
#			    ghc/lib/std/Makefile
#
#		Makefile for building the GHC Prelude libraries umpteen ways
#
#	
#################################################################################

TOP = ../..
include $(TOP)/mk/boilerplate.mk

WAYS=$(GhcLibWays)


ifeq "$(way)" ""
SUBDIRS = cbits
else
SUBDIRS=
endif

#-----------------------------------------------------------------------------
# 	Setting the standard variables
#

HC = $(GHC_INPLACE)

# *** THIS WON'T WORK ANY MORE *** (PACKAGE is now set in fptools/mk/target.mk)
ifeq "$(DLLized)" "YES"
# Hack by SPJ to delay if-then-else until the pattern rule when we have $*
PACKAGE = $(subst ~, ,$(word $(words dummy $(findstring $(notdir $*), PrelMain )), -package-name~std))
endif

PACKAGE = std

BOOT_SRCS += PrelPrimopWrappers.hs

#-----------------------------------------------------------------------------
# 	Setting the GHC compile options

# -fvia-C added because NCG still can't cope with some primops used in the standard library
SRC_HC_OPTS += -fvia-C -cpp -fglasgow-exts $(GhcLibHcOpts)
SRC_HSC2HS_OPTS += -Icbits

ifdef USE_REPORT_PRELUDE
SRC_HC_OPTS += -DUSE_REPORT_PRELUDE=1
endif

# ESSENTIAL, for getting reasonable performance from the I/O library:
PrelIOBase_HC_OPTS   = -funbox-strict-fields 

# debugging...
PrelIOBase_HC_OPTS   += -fno-ignore-asserts
PrelHandle_HC_OPTS   += -fno-ignore-asserts
PrelIO_HC_OPTS       += -fno-ignore-asserts

# Special options
PrelStorable_HC_OPTS = -monly-3-regs
PrelCError_HC_OPTS   = +RTS -K4m -RTS

#-----------------------------------------------------------------------------
# 	Dependency generation

SRC_MKDEPENDHS_OPTS += -I$(GHC_INCLUDE_DIR)

#-----------------------------------------------------------------------------
# 	Pre-processing (.pp) files
SRC_CPP_OPTS += -I$(GHC_INCLUDE_DIR) -traditional
SRC_CPP_OPTS += ${GhcLibCppOpts}

#-----------------------------------------------------------------------------
# 	Rules

PrelPrimopWrappers.hs: ../../compiler/prelude/primops.txt
	rm -f $@
	../../utils/genprimopcode/genprimopcode --make-haskell-wrappers < $< > $@

PrelGHC.$(way_)hi	: PrelGHC.hi-boot
	cp $< $@

boot :: PrelGHC.hi $(foreach way, $(WAYS), PrelGHC.$(way)_hi)

ifneq "$(BootingFromHc)" "YES"
boot :: PrelPrimopWrappers.hs
all  :: PrelPrimopWrappers.hs
endif

DLL_DESCRIPTION="GHC-compiled Haskell Prelude"

ifeq "$(DLLized)" "YES"
HS_SRCS := $(filter-out PrelMain.lhs, $(HS_SRCS))
# PrelMain.dll_o isn't to be included in the final .a, 
# but it needs to be generated
all :: PrelMain.dll_o
endif

CLEAN_FILES += PrelGHC.hi-boot PrelGHC.hi $(foreach way, $(WAYS), PrelGHC.$(way)_hi)


#-----------------------------------------------------------------------------
# 	Building the library for GHCi
#
# The procedure differs from that in fptools/mk/target.mk in two ways:
#  (a) we don't want PrelMain in the GHCi std library
#  (b) on Win32 we must split it into two, because a single .o file can't
#      have more than 65536 relocations in it.
#      

# we don't want PrelMain in the GHCi library.
GHCI_LIBOBJS = $(filter-out PrelMain.$(way_)o,$(HS_OBJS))


ifneq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
# 		Standard rule
HSstd.o :: $(GHCI_LIBOBJS)
	ld -r -x -o $@ $(GHCI_LIBOBJS)

else
# 		Rule for Win32 platform
# Keep HSstd.o as a pseudo-target (I think)

# Turn off standard rule which creates HSstd.o from LIBOBJS.
DONT_WANT_STD_GHCI_LIB_RULE=YES

HSstd.o :: $(GHCI_LIBOBJS)
	ld -r -x -o HSstd1.o $(filter     Prel%, $(GHCI_LIBOBJS))
	ld -r -x -o HSstd2.o $(filter-out Prel%, $(GHCI_LIBOBJS))
	@touch HSstd.o
endif # TARGETPLATFORM = i386-unknown-mingw32


#-----------------------------------------------------------------------------
# 	Installation; need to install .hi files as well as libraries
#
# The interface files are put inside the $(libdir), since they
# might (potentially) be platform specific..
#
# override is used here because for binary distributions, datadir is
# set on the command line. sigh.
#
override datadir:=$(libdir)/imports/std

#
# Files to install from here
# 
ifeq "$(DLLized)" "YES"
INSTALL_LIBS  += PrelMain.dll_o
endif

INSTALL_DATAS += PrelGHC.$(way_)hi



#-----------------------------------------------------------------------------
# ILX stuff.  PLEASE IGNORE THIS UNLESS YOU'RE WORKING ON GHC.NET


ilxstd:
	$(MAKE) way=i std.dll std.i_vlb
#	$(MAKE) way=ilx-Onot-mono std.ilx-Onot.mono.dll std.ilx-Onot.mono.vlb
#	$(MAKE) way=ilx-O-mono  std.ilx-O.mono.dll std.ilx-O.mono.vlb
#	$(MAKE) way=ilx-Onot-generic std.ilx-Onot.generic.dll
#	$(MAKE) way=ilx-O-generic  std.ilx-O.generic.dll
#	$(MAKE) way=ilx-Onot-mono-traced std.ilx-Onot.mono.dll std.ilx-Onot.mono-traced.vlb
#	$(MAKE) way=ilx-O-mono-traced  std.ilx-O.mono.dll std.ilx-O.mono-traced.vlb
#	$(MAKE) way=ilx-Onot-generic-traced std.ilx-Onot.generic-traced.dll
#	$(MAKE) way=ilx-O-generic-traced  std.ilx-O.generic-traced.dll
#	$(MAKE) way=ilx-Onot-mono-verifiable std.ilx-Onot.mono-verifiable.dll std.ilx-Onot.mono-verifiable.vlb
#	$(MAKE) way=ilx-O-mono-verifiable  std.ilx-O.mono-verifiable.dll std.ilx-O.mono-verifiable.vlb

ilxcheck:
#	(cd //c/devel/fcom/src; make)
#	(cd ../../compiler; make)
	$(MAKE) way=ilx-Onot-mono std.ilx-Onot.mono.mvl
	$(MAKE) way=ilx-O-mono std.ilx-O.mono.mvl
	$(MAKE) way=ilx-Onot-mono-verifiable std.ilx-Onot.mono-verifiable.mvl 
	$(MAKE) way=ilx-O-mono-verifiable std.ilx-O.mono-verifiable.mvl
	$(MAKE) way=ilx-Onot-mono-verifiable std.ilx-Onot.mono-verifiable.mvr 
	$(MAKE) way=ilx-O-mono-verifiable std.ilx-O.mono-verifiable.mvr


ifeq "$(ILXized)" "YES"

SRC_HC_OPTS += -optI--assembly-name -optIstd.$(way_)o -optI--module -DILX

HS_ILX+=PrelGHC.$(way_)o
PrelGHC.$(way_)o: PrelGHC.ilx.real
	sed -e "s/'PrelBase.dll'/'PrelBase.$(way_)o'/g" $< > $@.tmp
	$(ILX2IL) --module --assembly-name std.$(way_)o -o $@.il $@.tmp
	$(ILASM) /QUIET /DLL /OUT=$@ $@.il
#	mv $@.tmp $@

std.$(way_)mvl: $(HS_IL)
	((ILVALID_HOME=c:\\devel\\fcom\\src\\ ILVALID_MSCORLIB=mscorlib.vlb $(ILVALID) c:\\devel\\fcom\\src\\bin\\msilxlib$(ilx2il_suffix).vlb $(HS_IL)) 2>&1) | tee $@

std.$(way_)vlb: std.dll
	mkvlb.exe -V -o $@.tmp std
	cmd /c tmp.bat
	mv $@.tmp $@

MINI_IL=PrelBase.ilx-Onot.mono.il Prelude.ilx-Onot.mono.il PrelGHC.ilx-Onot.mono.il PrelPrimopWrappers.ilx-Onot.mono.il PrelErr.ilx-Onot.mono.il PrelIOBase.ilx-Onot.mono.il PrelTup.ilx-Onot.mono.il PrelShow.ilx-Onot.mono.il PrelList.ilx-Onot.mono.il PrelPtr.ilx-Onot.mono.il PrelMaybe.ilx-Onot.mono.il PrelPack.ilx-Onot.mono.il PrelST.ilx-Onot.mono.il PrelByteArr.ilx-Onot.mono.il PrelArr.ilx-Onot.mono.il PrelNum.ilx-Onot.mono.il PrelEnum.ilx-Onot.mono.il PrelFloat.ilx-Onot.mono.il PrelReal.ilx-Onot.mono.il PrelConc.ilx-Onot.mono.il
mini.mvl: $(MINI_IL)
	((ILVALID_HOME=c:\\devel\\fcom\\src\\ ILVALID_MSCORLIB=mscorlib.vlb $(ILVALID) c:\\devel\\fcom\\src\\bin\\msilxlib$(ilx2il_suffix).vlb  $(MINI_IL)) 2>&1) | tee $@


std.$(ilx_way).mvlx: $(HS_ILX)
	ILVALID_HOME=c:\\devel\\fcom\\src\\ ILVALID_MSCORLIB=mscorlib.vlb $(ILVALID) c:\\devel\\fcom\\src\\bin\\msilxlib.vlbx  $(HS_ILX) | tee $@

endif # ILXized

# End ILX stuff. 
#-----------------------------------------------------------------------------


include $(TOP)/mk/target.mk

# PrelIO.hsc includes PrelHandle_hsc.h
PrelIO.hsc : PrelHandle_hsc.h
