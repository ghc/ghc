
TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/cabal-flags.mk

SRC_HC_OPTS += -Wall

ifeq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
INSTALL_FLAGS =
else
INSTALL_FLAGS = --enable-shell-wrappers
endif

default all: with-bootstrapping-compiler

with-bootstrapping-compiler: Version.hs
	$(CABAL) configure --distpref dist-inplace         \
	                   $(INPLACE_DIRS_CONFIGURE_FLAGS) \
	                   $(USE_BOOT_CONFIGURE_FLAGS)     \
	                   $(COMMON_CONFIGURE_FLAGS)
	$(CABAL) build     --distpref dist-inplace $(BUILD_FLAGS)
	$(CABAL) install   --distpref dist-inplace $(INSTALL_FLAGS)

with-stage-1: Version.hs
	$(CABAL) configure --distpref dist-install         \
	                   $(INSTALL_DIRS_CONFIGURE_FLAGS) \
	                   $(USE_STAGE1_CONFIGURE_FLAGS)   \
	                   $(COMMON_CONFIGURE_FLAGS)
	$(CABAL) build     --distpref dist-install $(BUILD_FLAGS)

install:
	$(INSTALL_PACKAGE) install UNUSED UNUSED '$(DESTDIR)' '$(prefix)' \
	                   '$(prefix)' '$(bindir)' '$(libdir)'            \
                       '$(libexecdir)' '$(dynlibdir)' '$(datadir)'    \
                       '$(docdir)' '$(htmldir)' '$(haddockdir)'
	                   --distpref dist-install                        \
	                   $(INSTALL_FLAGS)

clean: distclean

distclean:
	-$(CABAL) clean --distpref dist-inplace
	-$(CABAL) clean --distpref dist-install
	$(RM) -f Version.hs

# XXX fix binary-dist

##### Here down is unique to ghc-pkg

Version.hs: Makefile $(TOP)/mk/config.mk
	$(RM) -f Version.hs
	echo "module Version where"                    >> Version.hs
	echo "version, targetOS, targetARCH :: String" >> Version.hs
	echo "version    = \"$(ProjectVersion)\""      >> Version.hs
	echo "targetOS   = \"$(TargetOS_CPP)\""        >> Version.hs
	echo "targetARCH = \"$(TargetArch_CPP)\""      >> Version.hs

