TOP=..
include $(TOP)/mk/boilerplate.mk

ALL_DIRS = \
	Data \
	Compat \
	Distribution \
	Distribution/Compat \
	Distribution/Simple \
	Language/Haskell \
	System \
	System/FilePath \
	System/Directory \
	Trace/Hpc \
	cbits

SplitObjs=NO
LIBRARY = libghccompat.a

# We don't want this installed
NO_INSTALL_LIBRARY = YES

# Avoid building the GHCi lib, since we don't need it
GhcWithInterpreter = NO

# There are lots of warnings in here due to things like modules being
# imported that, in some versions of GHC, aren't used. Thus we don't
# give any warnings in here, and therefore validating with -Werror won't
# make the build fail.
SRC_HC_OPTS += -w

# Needed so that the libraries can #include relative to this directory.
INCLUDE_DIRS=-I. -Iinclude

SRC_HC_OPTS    += $(INCLUDE_DIRS)
SRC_CC_OPTS    += $(INCLUDE_DIRS)
MKDEPENDC_OPTS += $(INCLUDE_DIRS)

# Just to silence warnings
MKDEPENDC_OPTS += -I$(GHC_INCLUDE_DIR)

UseGhcForCc = YES

# This library is linked to the compiler, at least in stage1, so we
# better make sure it is built the same "way".
#
# BUT, if GhcHcOpts includes -DDEBUG we *don't* want to compile
# lib/compat with -DDEBUG, because the preprocessor symbols used
# by the compiler may be understood differently by library code.
# In this particular case, it turned out that -DDEBUG made Cabal 
# import HUnit, which might not be installed for the compiler we are
# compiling with (e.g. 6.2.1). Hence the filter-out.
SRC_HC_OPTS += $(filter-out -D%, $(GhcHcOpts) $(GhcStage1HcOpts)) 

# GHC 6.4 didn't have WCsubst.c, but 6.4.1 did, and we need to know
# this in cbits/unicode.c The patchlevel isn't normally exposed as a
# CPP symbol, so we have to do it by hand:
SRC_CC_OPTS += -D__GHC_PATCHLEVEL__=$(GhcPatchLevel)

# This module shouldn't be compiled itself, only #included into other files
EXCLUDED_SRCS += System/FilePath/Internal.hs

ifneq "$(ghc_ge_607)" "YES"
# GHC 6.3+ has Cabal, but we're replacing it:
SRC_HC_OPTS += -ignore-package Cabal

# only GHC 6.3+ has -ignore-package
SRC_HC_OPTS += -ignore-package filepath
endif

ifeq "$(ghc_ge_607)" "YES"
SRC_HC_OPTS += -package directory
SRC_HC_OPTS += -package pretty
endif

# Some explicit dependencies, needed because ghc -M can't discover the
# true dependencies of these stub files.
System/Directory/Internals.$(way_)o : $(FPTOOLS_TOP)/libraries/directory/System/Directory/Internals.hs
Distribution/Compat/ReadP.$(way_) : $(FPTOOLS_TOP)/libraries/Cabal/Distribution/Compat/ReadP.hs
Distribution/GetOpt.$(way_)o    : $(FPTOOLS_TOP)/libraries/Cabal/Distribution/GetOpt.hs
Distribution/InstalledPackageInfo.$(way_)o :  $(FPTOOLS_TOP)/libraries/Cabal/Distribution/InstalledPackageInfo.hs
Distribution/License.$(way_)o :  $(FPTOOLS_TOP)/libraries/Cabal/Distribution/License.hs
Distribution/Package.$(way_)o :  $(FPTOOLS_TOP)/libraries/Cabal/Distribution/Package.hs
Distribution/ParseUtils.$(way_)o :  $(FPTOOLS_TOP)/libraries/Cabal/Distribution/ParseUtils.hs
Distribution/Compiler.$(way_)o :  $(FPTOOLS_TOP)/libraries/Cabal/Distribution/Compiler.hs
Distribution/Version.$(way_)o :  $(FPTOOLS_TOP)/libraries/Cabal/Distribution/Version.hs
Language/Haskell/Extension.$(way_)o :  $(FPTOOLS_TOP)/libraries/Cabal/Language/Haskell/Extension.hs
Trace/Hpc/Mix.$(way_)o :  $(FPTOOLS_TOP)/libraries/hpc/Trace/Hpc/Mix.hs
Trace/Hpc/Util.$(way_)o :  $(FPTOOLS_TOP)/libraries/hpc/Trace/Hpc/Util.hs
Trace/Hpc/Tix.$(way_)o :  $(FPTOOLS_TOP)/libraries/hpc/Trace/Hpc/Tix.hs

cbits/unicode.o : $(FPTOOLS_TOP)/libraries/base/cbits/WCsubst.c $(FPTOOLS_TOP)/libraries/base/include/WCsubst.h

SRC_CC_OPTS += -I$(FPTOOLS_TOP)/libraries/base/cbits -I$(FPTOOLS_TOP)/libraries/base/include

# Make the #includes in the stubs independent of the current location
SRC_HC_OPTS += -I$(FPTOOLS_TOP)/libraries

SRC_HC_OPTS +=  -fglasgow-exts -fforce-recomp

ifeq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
Compat/Directory_HC_OPTS += -\#include shlobj.h
endif

# libghccompat is needed to build ghc-pkg, which is built during 'make boot',
# so we must build this library during 'make boot' too.
# Do a recursive 'make all' after generating dependencies, because this
# will work with 'make -j'.
ifneq "$(BootingFromHc)" "YES"
boot :: depend
	$(MAKE) all
endif

# We don't ever want to build libghccompat as a shared library.
GhcBuildDylibs=NO

include $(TOP)/mk/target.mk
