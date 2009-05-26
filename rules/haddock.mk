# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------


define haddock  # args: $1 = dir,  $2 = distdir, $3 = way

ifneq "$$($1_$2_DO_HADDOCK)" "NO"

ifeq "$$($$($1_PACKAGE)_HADDOCK_FILE)" ""
$$($1_PACKAGE)_HADDOCK_FILE = $1/$2/doc/html/$$($1_PACKAGE)/$$($1_PACKAGE).haddock
else
$$(error Already got a haddock file for $$($1_PACKAGE))
endif

ifeq "$$(HADDOCK_DOCS)" "YES"
$(call all-target,$1_$2_haddock,$$($$($1_PACKAGE)_HADDOCK_FILE))
endif

$$($1_PACKAGE)_HADDOCK_DEPS = $$(foreach n,$$($1_$2_DEP_NAMES),$$($$n_HADDOCK_FILE))

ifeq "$$(HSCOLOUR_SRCS)" "YES"
$1_$2_HADDOCK_FLAGS += --hyperlink-source
endif

$$($$($1_PACKAGE)_HADDOCK_FILE) : $$(MKDIRHIER) $(INPLACE_BIN)/haddock$(exeext) $$(GHC_CABAL_INPLACE) $$($1_$2_HS_SRCS) $$($$($1_PACKAGE)_HADDOCK_DEPS)
	$$(MKDIRHIER) $$(dir $$@)
	$$(GHC_CABAL_INPLACE) haddock $2 $1 --with-haddock=$$(TOP)/$(INPLACE_BIN)/haddock --with-ghc=$$(TOP)/$(INPLACE_BIN)/ghc-stage2 $$($1_$2_HADDOCK_FLAGS) $$($1_$2_HADDOCK_OPTS)

endif

endef

