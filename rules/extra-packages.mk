# -----------------------------------------------------------------------------
#
# (c) 2010 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------

define extra-packages
$$(foreach p,$$(patsubst libraries/%,%,$$(wildcard $$(shell grep '^[^ ]\+ \+\(dph\|extra\) \+[^ ]\+ \+[^ ]\+ \+[^ ]\+' packages | sed 's/ .*//'))),\
    $$(eval BUILD_DIRS += libraries/$$p)\
    $$(if $$(wildcard libraries/$$p/ghc-packages),\
        $$(foreach q,$$(shell cat libraries/$$p/ghc-packages2),$$(eval $$(call extra-package,$$p,$$p/$$q))),\
        $$(eval $$(call extra-package,$$p,$$p)))\
)
endef

define extra-package # $1 = package root, $2 = package
ifeq "$(wildcard libraries/$1/ghc-stage2-package)" ""
$$(eval $$(call addPackage,$2))
else
$$(eval $$(call addPackage2,$2))
endif
endef

