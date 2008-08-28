
TOP=../..
ENABLE_SHELL_WRAPPERS = YES
EXTRA_CLEAN = Version.hs
EXTRA_INPLACE_CONFIGURE_FLAGS = $(INPLACE_GHC_DATADIR_CONFIGURE_FLAGS)
EXTRA_STAGE2_CONFIGURE_FLAGS = --datasubdir=.

include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/cabal.mk

with-bootstrapping-compiler: Version.hs
with-stage-2: Version.hs

Version.hs: Makefile $(TOP)/mk/config.mk
	$(RM) -f Version.hs
	echo "module Version where"                    >> Version.hs
	echo "version, targetOS, targetARCH :: String" >> Version.hs
	echo "version    = \"$(ProjectVersion)\""      >> Version.hs
	echo "targetOS   = \"$(TargetOS_CPP)\""        >> Version.hs
	echo "targetARCH = \"$(TargetArch_CPP)\""      >> Version.hs

