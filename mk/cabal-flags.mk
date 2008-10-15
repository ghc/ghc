
nothing=
space=$(nothing) $(nothing)
comma=,

GHC_PKG_INSTALL_PROG = $(FPTOOLS_TOP_ABS)/utils/ghc-pkg/dist-install/build/ghc-pkg/ghc-pkg

LIBRARIES_ABS = $(FPTOOLS_TOP_ABS)/libraries
UTILS_ABS     = $(FPTOOLS_TOP_ABS)/utils
CABAL = $(LIBRARIES_ABS)/cabal-bin $(GHC) $(LIBRARIES_ABS)/bootstrapping.conf
INSTALL_PACKAGE = \
    $(UTILS_ABS)/installPackage/install-inplace/bin/installPackage
STAGE3_PACKAGE_CONF = $(FPTOOLS_TOP_ABS)/stage3.package.conf

# Euch, hideous hack:
CABAL_DOTTED_VERSION = $(shell grep "^Version:" $(LIBRARIES_ABS)/Cabal/Cabal.cabal | sed "s/^Version: //")
CABAL_VERSION = $(subst .,$(comma),$(CABAL_DOTTED_VERSION))
CABAL_CONSTRAINT = --constraint="Cabal == $(CABAL_DOTTED_VERSION)"

# We rely on all the CONFIGURE_ARGS being quoted with '...', and there
# being no 's inside the values.
FLAGGED_CONFIGURE_ARGS = $(subst $(space)',\
                                 $(space)--configure-option=',\
                                 $(space)$(CONFIGURE_ARGS))

COMMON_CONFIGURE_FLAGS =          \
    --libsubdir='$$pkgid'         \
    --with-gcc=$(WhatGccIsCalled) \
    --with-ld=$(LD)               \
    $(addprefix --hsc2hs-option=,$(SRC_HSC2HS_OPTS))

ifneq "$(HSCOLOUR)" ""
COMMON_CONFIGURE_FLAGS += --with-hscolour=$(HSCOLOUR)
endif

ifneq "$(ALEX)" ""
COMMON_CONFIGURE_FLAGS += --with-alex=$(ALEX)
endif

ifneq "$(HAPPY)" ""
COMMON_CONFIGURE_FLAGS += --with-happy=$(HAPPY)
endif

COMMON_CONFIGURE_FLAGS += $(FLAGGED_CONFIGURE_ARGS) \
                          --configure-option=--with-cc=$(CC)

COMMON_CONFIGURE_FLAGS += $(shell [ -f $(HSC2HS_INPLACE) ] && \
                                  echo --with-hsc2hs=$(HSC2HS_INPLACE) )

ifeq "$(Windows)" "YES"
NONEXISTENT=c:/NONEXISTENT
else
NONEXISTENT=/NONEXISTENT
endif

# We put non-existant paths in when configuring things that we plan to
# install, as we require that builds don't depend on these paths when
# making bindists.
INSTALL_DIRS_CONFIGURE_FLAGS = \
    --prefix=$(NONEXISTENT) \
    --bindir=$(NONEXISTENT) \
    --libdir=$(NONEXISTENT) \
    --libexecdir=$(NONEXISTENT) \
    --datadir=$(NONEXISTENT) \
    --docdir=$(NONEXISTENT) \
    --haddockdir=$(NONEXISTENT) \
    --htmldir=$(NONEXISTENT)

INPLACE_GHC_DATADIR_CONFIGURE_FLAGS = \
    --datadir=$(FPTOOLS_TOP_ABS)/inplace-datadir \
    --datasubdir=.

INPLACE_DIRS_CONFIGURE_FLAGS = \
    --prefix=`$(FPTOOLS_TOP_ABS)/utils/pwd/pwd forwardslash`/install-inplace

USE_BOOT_CONFIGURE_FLAGS = \
    --with-compiler=$(GHC) \
    --with-hc-pkg=$(GHC_PKG) \
	--package-db $(FPTOOLS_TOP_ABS)/libraries/bootstrapping.conf

USE_STAGE_CONFIGURE_FLAGS = \
    --with-hc-pkg=$(GHC_PKG_INPLACE) \
    $(addprefix --cc-option=,$(MACOSX_DEPLOYMENT_CC_OPTS)) \
    $(addprefix --ld-option=,$(MACOSX_DEPLOYMENT_LD_OPTS))

USE_STAGE1_CONFIGURE_FLAGS = \
    --with-compiler=$(GHC_STAGE1) \
    $(USE_STAGE_CONFIGURE_FLAGS)

USE_STAGE2_CONFIGURE_FLAGS =            \
    --with-compiler=$(GHC_STAGE2)       \
    $(USE_STAGE_CONFIGURE_FLAGS)        \
	--package-db $(STAGE3_PACKAGE_CONF)

BUILD_FLAGS = $(addprefix --ghc-option=,$(SRC_HC_OPTS))


# We now define various i* variables, which are used for the paths
# when installing Cabal libraries
ifeq "$(RelocatableBuild)" "YES"
# On Windows we want to make moveable bindists, but we need to tell
# ghc-pkg where the haddock docs are. Therefore we completely ignore
# where the user tells us to put the haddock documentation and put it
# somewhere whose relative location we know. When installing we need
# to give Cabal a real path, though.
iprefix             = $$topdir
ibindir             = $$topdir
ilibdir             = $$topdir
ilibexecdir         = $$topdir
idynlibdir          = $$topdir
idatadir            = $$topdir
idocdir             = $$topdir/doc/libraries/$$pkg
ihaddockdir         = $$topdir/doc/libraries/$$pkg
ihtmldir            = $$httptopdir/doc/libraries/$$pkg
html_installed_root = $(prefix)/doc/libraries
else
# On non-Windows we can just give absolute paths all the time, and
# thus obey the htmldir that we are given.
iprefix             = $(prefix)
ibindir             = $(bindir)
ilibdir             = $(libdir)
ilibexecdir         = $(libexecdir)
idynlibdir          = $(dynlibdir)
idatadir            = $(datadir)
idocdir             = $(docdir)/libraries/$$pkg
ihaddockdir         = $(htmldir)/libraries/$$pkg
ihtmldir            = $(htmldir)/libraries/$$pkg
html_installed_root = $(htmldir)/libraries
endif

