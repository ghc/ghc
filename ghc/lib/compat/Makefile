TOP=../..
include $(TOP)/mk/boilerplate.mk

ALL_DIRS = \
	Data \
	Compat \
	Distribution \
	Distribution/Compat \
	System \
	System/Directory \
	cbits

LIBRARY = libghccompat.a

# Just to silence warnings
MKDEPENDC_OPTS += -I$(GHC_INCLUDE_DIR)

UseGhcForCc = YES

# This library is linked to the compiler, at least in stage1, so we
# better make sure it is built the same "way":
SRC_HC_OPTS += $(GhcHcOpts)

ifeq "$(ghc_ge_603)" "YES"
# These modules are all provided in GHC 6.3+
EXCLUDED_SRCS += \
	System/Directory/Internals.hs \
	Distribution/Compat/ReadP.hs \
	Distribution/Extension.hs \
	Distribution/GetOpt.hs \
	Distribution/InstalledPackageInfo.hs \
	Distribution/License.hs \
	Distribution/Package.hs \
	Distribution/ParseUtils.hs \
	Distribution/Setup.hs \
	Distribution/Version.hs
endif

# Some explicit dependencies
System/Directory/Internals.$(way_)o : $(FPTOOLS_TOP)/libraries/base/System/Directory/Internals.hs
Distribution/Compat/ReadP.$(way_) : $(FPTOOLS_TOP)/libraries/Cabal/Distribution/Compat/ReadP.hs
Distribution/Extension.$(way_)o    : $(FPTOOLS_TOP)/libraries/Cabal/Distribution/Extension.hs
Distribution/GetOpt.$(way_)o    : $(FPTOOLS_TOP)/libraries/Cabal/Distribution/GetOpt.hs
Distribution/InstalledPackageInfo.$(way_)o :  $(FPTOOLS_TOP)/libraries/Cabal/Distribution/InstalledPackageInfo.hs
Distribution/License.$(way_)o :  $(FPTOOLS_TOP)/libraries/Cabal/Distribution/License.hs
Distribution/Package.$(way_)o :  $(FPTOOLS_TOP)/libraries/Cabal/Distribution/Package.hs
Distribution/ParseUtils.$(way_)o :  $(FPTOOLS_TOP)/libraries/Cabal/Distribution/ParseUtils.hs
Distribution/Setup.$(way_)o :  $(FPTOOLS_TOP)/libraries/Cabal/Distribution/Setup.hs
Distribution/Version.$(way_)o :  $(FPTOOLS_TOP)/libraries/Cabal/Distribution/Version.hs

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
