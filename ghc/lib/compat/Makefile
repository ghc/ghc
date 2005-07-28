TOP=../..
include $(TOP)/mk/boilerplate.mk

ALL_DIRS = \
	Data \
	Compat \
	Distribution \
	Distribution/Compat \
	Language/Haskell \
	System \
	System/Directory \
	cbits

SplitObjs=NO
LIBRARY = libghccompat.a

# We don't want this installed
NO_INSTALL_LIBRARY = YES

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
SRC_HC_OPTS += $(filter-out -D%, $(GhcHcOpts)) 

ifeq "$(ghc_ge_603)" "YES"
# These modules are all provided in GHC 6.3+
EXCLUDED_SRCS += \
	System/Directory/Internals.hs \
	Distribution/Compat/FilePath.hs \
	Distribution/Compat/ReadP.hs \
	Distribution/GetOpt.hs \
	Distribution/InstalledPackageInfo.hs \
	Distribution/License.hs \
	Distribution/Package.hs \
	Distribution/ParseUtils.hs \
	Distribution/Compiler.hs \
	Distribution/Version.hs \
	Language/Haskell/Extension.hs

SRC_MKDEPENDHS_OPTS += \
	-optdep--exclude-module=System.Directory.Internals \
	-optdep--exclude-module=Distribution.Compat.FilePath \
	-optdep--exclude-module=Distribution.Compat.ReadP \
	-optdep--exclude-module=Distribution.GetOpt \
	-optdep--exclude-module=Distribution.InstalledPackageInfo \
	-optdep--exclude-module=Distribution.License \
	-optdep--exclude-module=Distribution.Package \
	-optdep--exclude-module=Distribution.ParseUtils \
	-optdep--exclude-module=Distribution.Compiler \
	-optdep--exclude-module=Distribution.Version \
	-optdep--exclude-module=Language.Haskell.Extension
endif

# Some explicit dependencies
System/Directory/Internals.$(way_)o : $(FPTOOLS_TOP)/libraries/base/System/Directory/Internals.hs
Distribution/Compat/ReadP.$(way_) : $(FPTOOLS_TOP)/libraries/Cabal/Distribution/Compat/ReadP.hs
Distribution/GetOpt.$(way_)o    : $(FPTOOLS_TOP)/libraries/Cabal/Distribution/GetOpt.hs
Distribution/InstalledPackageInfo.$(way_)o :  $(FPTOOLS_TOP)/libraries/Cabal/Distribution/InstalledPackageInfo.hs
Distribution/License.$(way_)o :  $(FPTOOLS_TOP)/libraries/Cabal/Distribution/License.hs
Distribution/Package.$(way_)o :  $(FPTOOLS_TOP)/libraries/Cabal/Distribution/Package.hs
Distribution/ParseUtils.$(way_)o :  $(FPTOOLS_TOP)/libraries/Cabal/Distribution/ParseUtils.hs
Distribution/Compiler.$(way_)o :  $(FPTOOLS_TOP)/libraries/Cabal/Distribution/Compiler.hs
Distribution/Version.$(way_)o :  $(FPTOOLS_TOP)/libraries/Cabal/Distribution/Version.hs
Language/Haskell/Extension.$(way_)o :  $(FPTOOLS_TOP)/libraries/Cabal/Language/Haskell/Extension.hs

# Make the #includes in the stubs independent of the current location
SRC_HC_OPTS += -I$(FPTOOLS_TOP)/libraries

SRC_HC_OPTS +=  -fglasgow-exts -no-recomp

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
