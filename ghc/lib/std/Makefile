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

HC 	   = $(GHC_INPLACE)
MKDEPENDHS = $(GHC_INPLACE)

ifneq "$(way)" "dll"
PACKAGE = -package-name std
else
# Hack by SPJ to delay if-then-else until the pattern rule when we have $*
PACKAGE = $(subst ~, ,$(word $(words dummy $(findstring $(notdir $*), PrelMain )), -package-name~std))
endif
LIBRARY = libHSstd$(_way).a

LIBOBJS = $(HS_OBJS)
ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
LIBOBJS = $(filter-out PrelHugs.$(way_)o,$(HS_OBJS))
endif

#-----------------------------------------------------------------------------
# 	Setting the GHC compile options

SRC_HC_OPTS += -recomp -cpp -fglasgow-exts -fvia-C -Rghc-timing $(GhcLibHcOpts) $(PACKAGE)
ifneq "$(way)" "dll"
SRC_HC_OPTS += -static
endif

ifdef USE_REPORT_PRELUDE
SRC_HC_OPTS += -DUSE_REPORT_PRELUDE=1
endif

#
# Object and interface files have suffixes tagged with their ways
#
ifneq "$(way)" ""
SRC_HC_OPTS += -hisuf $(way_)hi
endif

# Far too much heap is needed to compile PrelNumExtra with -O at the
# moment, but there you go..
PrelNumExtra_HC_OPTS     += -H24m -K2m

PrelPack_HC_OPTS	 += -K4m
PrelBase_HC_OPTS         += -H12m
PrelRead_HC_OPTS         += -H20m
PrelTup_HC_OPTS          += -H12m -K2m
PrelNum_HC_OPTS		 += -H12m -K4m
PrelArr_HC_OPTS          += -H8m
PrelHandle_HC_OPTS       += -H20m
Time_HC_OPTS             += -H24m -K2m
Complex_HC_OPTS          += -H10m
IO_HC_OPTS	         += -H12m
List_HC_OPTS             += -H8m
Directory_HC_OPTS        += -H8m

#-----------------------------------------------------------------------------
# 	Dependency generation

SRC_MKDEPENDHS_OPTS += -I$(GHC_INCLUDE_DIR)

#-----------------------------------------------------------------------------
# 	Rules

PrelGHC.$(way_)hi	: PrelGHC.hi-boot
	cp $< $@

boot :: PrelGHC.hi $(foreach way, $(WAYS), PrelGHC.$(way)_hi)

DLL_NAME = HSstd.dll
DLL_DESCRIPTION="GHC-compiled Haskell Prelude"
DLL_IMPLIB_NAME = libHSstd_imp.a
SRC_BLD_DLL_OPTS += --export-all --output-def=HSstd.def DllVersionInfo.o
SRC_BLD_DLL_OPTS += -lwinmm -lHSrts_imp -lHSstd_cbits_imp -lgmp -L. -L../../rts/gmp -L../../rts -Lcbits

ifeq "$(way)" "dll"
HS_SRCS := $(filter-out PrelMain.lhs PrelHugs.lhs, $(HS_SRCS))
endif

# PrelMain.dll_o isn't to be included in the final .a, 
# but it needs to be generated
ifeq "$(way)" "dll"
all :: PrelMain.dll_o DllVersionInfo.o

endif

CLEAN_FILES += PrelGHC.hi $(foreach way, $(WAYS), PrelGHC.$(way)_hi)

#
# If we're building the unregisterised way, it may well be for Hugs.
# In that case, remember to bind the cbits objects into a single file
# which hugs can load as an auxiliary object file when loading the Prelude.
#
ifeq "$(way)" "u"
all :: libHSstd_cbits.u_o
CLEAN_FILES += libHSstd_cbits.u_o
libHSstd_cbits.u_o:
	$(RM) libHSstd_cbits.u_o
	ld -r -o libHSstd_cbits.u_o cbits/*.o
endif

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
INSTALL_LIBS  += $(LIBRARY)
ifeq "$(EnableWin32DLLs)" "YES"
INSTALL_PROGS += $(DLL_NAME)
INSTALL_LIBS  += $(patsubst %.a, %_imp.a, $(LIBRARY)) PrelMain.dll_o
INSTALL_DATAS += dLL_ifs.hi
endif
INSTALL_DATAS += $(HS_IFACES) PrelGHC.$(way_)hi

include $(TOP)/mk/target.mk
