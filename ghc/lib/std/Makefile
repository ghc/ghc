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

ifneq "$(DLLized)" "YES"
PACKAGE = -package-name std
else
# Hack by SPJ to delay if-then-else until the pattern rule when we have $*
PACKAGE = $(subst ~, ,$(word $(words dummy $(findstring $(notdir $*), PrelMain )), -package-name~std))
endif

HSLIB = std

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
LIBOBJS = $(filter-out PrelHugs.$(way_)o,$(HS_OBJS))
endif

#-----------------------------------------------------------------------------
# 	Setting the GHC compile options

SRC_HC_OPTS += -recomp -cpp -fvia-C -fglasgow-exts $(GhcLibHcOpts) $(PACKAGE)
ifneq "$(DLLized)" "YES"
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
<<<<<<< Makefile
# PrelNumExtra_HC_OPTS     += -H24m -K2m
# 
# PrelPack_HC_OPTS	 += -K4m
# PrelBase_HC_OPTS         += -H12m
# PrelRead_HC_OPTS         += -H20m
# PrelTup_HC_OPTS          += -H12m -K2m
# PrelNum_HC_OPTS		 += -H12m -K4m
# PrelArr_HC_OPTS          += -H8m
# PrelHandle_HC_OPTS       += -H20m
# Time_HC_OPTS             += -H24m -K2m
# Complex_HC_OPTS          += -H10m
# IO_HC_OPTS	         += -H12m
# List_HC_OPTS             += -H8m
# Directory_HC_OPTS        += -H8m
=======
PrelNumExtra_HC_OPTS     += +RTS -H24m -K2m -RTS
>>>>>>> 1.50

<<<<<<< Makefile
=======
PrelPack_HC_OPTS	 += +RTS -K4m -RTS
PrelBase_HC_OPTS         += +RTS -H12m -RTS
PrelRead_HC_OPTS         += +RTS -H20m -RTS
PrelTup_HC_OPTS          += +RTS -H12m -K2m -RTS
PrelNum_HC_OPTS		 += +RTS -H12m -K4m -RTS
PrelArr_HC_OPTS          += +RTS -H8m -RTS
PrelHandle_HC_OPTS       += +RTS -H20m -RTS
Time_HC_OPTS             += +RTS -H24m -K2m -RTS
Complex_HC_OPTS          += +RTS -H10m -RTS
IO_HC_OPTS	         += +RTS -H12m -RTS
List_HC_OPTS             += +RTS -H8m -RTS
Directory_HC_OPTS        += +RTS -H8m -RTS

>>>>>>> 1.50
#-----------------------------------------------------------------------------
# 	Dependency generation

SRC_MKDEPENDHS_OPTS += -I$(GHC_INCLUDE_DIR)

#-----------------------------------------------------------------------------
# 	Rules

PrelGHC.$(way_)hi	: PrelGHC.hi-boot
	cp $< $@

boot :: PrelGHC.hi $(foreach way, $(WAYS), PrelGHC.$(way)_hi)

DLL_DESCRIPTION="GHC-compiled Haskell Prelude"

ifeq "$(DLLized)" "YES"
HS_SRCS := $(filter-out PrelMain.lhs PrelHugs.lhs, $(HS_SRCS))
endif

# PrelMain.dll_o isn't to be included in the final .a, 
# but it needs to be generated
ifeq "$(DLLized)" "YES"
all :: PrelMain.dll_o
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
ifeq "$(DLLized)" "YES"
INSTALL_LIBS  += PrelMain.dll_o
endif

INSTALL_DATAS += PrelGHC.$(way_)hi
ifeq "$(DLLized)" "YES"
INSTALL_DATAS := $(filter-out PrelHugs.$(way_)hi,$(INSTALL_DATAS))
endif

include $(TOP)/mk/target.mk
