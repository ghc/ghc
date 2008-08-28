
include $(TOP)/mk/cabal-flags.mk
include $(TOP)/mk/bindist.mk

SRC_HC_OPTS      += -Wall

ifeq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
INSTALL_FLAGS =
else
ifeq "$(ENABLE_SHELL_WRAPPERS)" "YES"
INSTALL_FLAGS = --enable-shell-wrappers
else
INSTALL_FLAGS =
endif
endif

# Tell stage2 to make a dynamically-linked binary, but no wrapper.  We assume
# that in an installation the shared libs will be installed somewhere that
# the system can find them.
ifeq "$(BuildSharedLibs)" "YES"
DYN_FLAGS = --ghc-option=-dynamic --ghc-option=-dynload --ghc-option=deploy
endif

.PHONY: default all with-bootstrapping-compiler with-stage-2 clean distclean

default all: with-bootstrapping-compiler

with-bootstrapping-compiler:
	$(CABAL) configure --distpref dist-inplace          \
	                   $(INPLACE_DIRS_CONFIGURE_FLAGS)  \
	                   $(USE_BOOT_CONFIGURE_FLAGS)      \
	                   $(COMMON_CONFIGURE_FLAGS)        \
	                   $(EXTRA_INPLACE_CONFIGURE_FLAGS)
	$(CABAL) build     --distpref dist-inplace $(BUILD_FLAGS)
	$(CABAL) install   --distpref dist-inplace $(INSTALL_FLAGS)

with-stage-2:
	$(CABAL) configure --distpref dist-install         \
	                   $(INSTALL_DIRS_CONFIGURE_FLAGS) \
	                   $(USE_STAGE2_CONFIGURE_FLAGS)   \
	                   $(COMMON_CONFIGURE_FLAGS)       \
	                   $(EXTRA_STAGE2_CONFIGURE_FLAGS)
	$(CABAL) build     --distpref dist-install $(DYN_FLAGS) $(BUILD_FLAGS)

install:
	$(INSTALL_PACKAGE) install                                        \
	                   '$(GHC_PKG_INSTALL_PROG)'                      \
	                   '$(DESTDIR)$(datadir)/package.conf'            \
	                   '$(DESTDIR)' '$(prefix)'                       \
	                   '$(prefix)' '$(bindir)' '$(libdir)'            \
	                   '$(libexecdir)' '$(dynlibdir)' '$(datadir)'    \
	                   '$(docdir)' '$(htmldir)' '$(haddockdir)'       \
	                   --distpref dist-install                        \
	                   $(INSTALL_FLAGS)

clean:
	-$(CABAL) clean --distpref dist-inplace
	-$(CABAL) clean --distpref dist-install
	$(RM) -rf install-inplace
ifneq "$(EXTRA_CLEAN)" ""
	$(RM) -f $(EXTRA_CLEAN)
endif

distclean: clean
ifneq "$(EXTRA_DISTCLEAN)" ""
	$(RM) -f $(EXTRA_DISTCLEAN)
endif

