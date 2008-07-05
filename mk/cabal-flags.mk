
nothing=
space=$(nothing) $(nothing)

# We rely on all the CONFIGURE_ARGS being quoted with '...', and there
# being no 's inside the values.
FLAGGED_CONFIGURE_ARGS = $(subst $(space)',\
                                 $(space)--configure-option=',\
                                 $(space)$(CONFIGURE_ARGS))

COMMON_CONFIGURE_FLAGS = \
    --libsubdir='$$pkgid' \
    --with-ld=$(LD) \
    $(addprefix --hsc2hs-option=,$(SRC_HSC2HS_OPTS))

ifneq "$(HSCOLOUR)" ""
COMMON_CONFIGURE_FLAGS += --with-hscolour=$(HSCOLOUR)
endif

ifneq "$(ALEX)" ""
COMMON_CONFIGURE_FLAGS += --with-alex=$(ALEX)
endif

ifneq "$(HADDOCK)" ""
COMMON_CONFIGURE_FLAGS += --with-haddock=$(HADDOCK)
endif

ifneq "$(HAPPY)" ""
COMMON_CONFIGURE_FLAGS += --with-happy=$(HAPPY)
endif

COMMON_CONFIGURE_FLAGS += $(FLAGGED_CONFIGURE_ARGS) \
                          --configure-option=--with-cc=$(CC)

# We put non-existant paths in when configuring things that we plan to
# install, as we require that builds don't depend on these paths when
# making bindists.
INSTALL_CONFIGURE_FLAGS = \
    --prefix=$(NONEXISTENT) \
    --bindir=$(NONEXISTENT) \
    --libdir=$(NONEXISTENT) \
    --libexecdir=$(NONEXISTENT) \
    --datadir=$(NONEXISTENT) \
    --docdir=$(NONEXISTENT) \
    --haddockdir=$(NONEXISTENT) \
    --htmldir=$(NONEXISTENT) \
    --with-compiler=$(FPTOOLS_TOP_ABS)/compiler/stage1/ghc-inplace \
    --with-hc-pkg=$(FPTOOLS_TOP_ABS)/utils/ghc-pkg/ghc-pkg-inplace \
    --with-hsc2hs=$(HSC2HS_INPLACE) \
    $(COMMON_CONFIGURE_FLAGS)

INPLACE_CONFIGURE_FLAGS = \
    --prefix=`$(TOP)/utils/pwd/pwd forwardslash`/install-inplace \
    --with-compiler=$(GHC) \
    --with-hc-pkg=$(GHC_PKG) \
    $(COMMON_CONFIGURE_FLAGS)

# XXX
#    --bindir='$$prefix/bin' \
#    --libdir='$$prefix/lib' \
#    --libexecdir='$$prefix/libexec' \
#    --datadir='$$prefix/data' \
#    --docdir='$$prefix/doc' \
#    --haddockdir='$$prefix/haddock' \
#    --htmldir='$$prefix/html' \

BUILD_FLAGS = $(addprefix --ghc-option=,$(SRC_HC_OPTS))

