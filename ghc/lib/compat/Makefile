TOP=../..
include $(TOP)/mk/boilerplate.mk

ALL_DIRS = \
	Data \
	Compat \
	Distribution \
	Distribution/Compat \
	cbits

LIBRARY = libghccompat.a

# Just to silence warnings
MKDEPENDC_OPTS += -I$(GHC_INCLUDE_DIR)

UseGhcForCc = YES

ghc_603_plus = $(shell if (test $(GhcCanonVersion) -ge 603); then echo YES; else echo NO; fi)

ifeq "$(ghc_603_plus)" "YES"
# These modules are all provided in GHC 6.3+
EXCLUDED_SRCS += \
	Data/Version.hs \
	Distribution/Compat/Error.hs \
	Distribution/Compat/ReadP.hs \
	Distribution/Extension.hs \
	Distribution/InstalledPackageInfo.hs \
	Distribution/License.hs \
	Distribution/Package.hs \
	Distribution/ParseUtils.hs \
	Distribution/Setup.hs \
	Distribution/Version.hs
endif

# Make the #includes in the stubs independent of the current location
SRC_HC_OPTS += -I$(FPTOOLS_TOP)/libraries

SRC_HC_OPTS +=  -fglasgow-exts

# libghccompat is needed to build ghc-pkg, which is built during 'make boot',
# so we must build this library during 'make boot' too.
# Do a recursive 'make all' after generating dependencies, because this
# will work with 'make -j'.
ifneq "$(BootingFromHc)" "YES"
boot :: depend
	$(MAKE) all
endif

include $(TOP)/mk/target.mk
