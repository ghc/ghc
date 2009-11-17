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


define haddock  # args: $1 = dir,  $2 = distdir

ifneq "$$($1_$2_DO_HADDOCK)" "NO"

ifeq "$$($$($1_PACKAGE)_HADDOCK_FILE)" ""
$$($1_PACKAGE)_HADDOCK_FILE = $1/$2/doc/html/$$($1_PACKAGE)/$$($1_PACKAGE).haddock
ALL_HADDOCK_FILES += $$($$($1_PACKAGE)_HADDOCK_FILE)
else
$$(error Already got a haddock file for $$($1_PACKAGE))
endif

haddock: $$($$($1_PACKAGE)_HADDOCK_FILE)

ifeq "$$(HADDOCK_DOCS)" "YES"
$(call all-target,$1_$2_haddock,html_$1)
endif

.PHONY: html_$1
html_$1 : $$($$($1_PACKAGE)_HADDOCK_FILE)

$$($1_PACKAGE)_HADDOCK_DEPS = $$(foreach n,$$($1_$2_DEP_NAMES),$$($$n_HADDOCK_FILE))

ifeq "$$(HSCOLOUR_SRCS)" "YES"
$1_$2_HADDOCK_FLAGS += --hyperlink-source
endif

ifneq "$$(BINDIST)" "YES"
$$($$($1_PACKAGE)_HADDOCK_FILE) : $$(INPLACE_BIN)/haddock$$(exeext) $$(GHC_CABAL_INPLACE) $$($1_$2_HS_SRCS) $$($$($1_PACKAGE)_HADDOCK_DEPS) | $$$$(dir $$$$@)/.
	"$$(GHC_CABAL_INPLACE)" haddock $2 $1 --with-haddock=$$(TOP)/$$(INPLACE_BIN)/haddock --with-ghc=$$(TOP)/$$(INPLACE_BIN)/ghc-stage2 $$($1_$2_HADDOCK_FLAGS) $$($1_$2_HADDOCK_OPTS)

# Make the haddocking depend on the library .a file, to ensure
# that we wait until the library is fully build before we haddock it
$$($$($1_PACKAGE)_HADDOCK_FILE) : $$($1_$2_v_LIB)
endif

endif

endef

