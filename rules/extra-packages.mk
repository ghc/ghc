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

# For each package P marked as "dph" or "extra" in $(TOP)/packages:
#   if $(TOP)/libraries/P exists, then
#      if $(TOP)/libraries/P/ghc-packages exists, then
#         * add each package from $(TOP)/libraries/P/ghc-packages2 to the list of
#	    packages.
#           Note: ghc-packages2 might have a different list from
#	    ghc-packages, this is to support dph which has some
#	    packages that are automatically derived from a single
#	    source by the build system).
#         * add $(TOP)/libraries/P to $(BUILD_DIRS)
#           This step is necessary in the case of dph, which has some
#           build system code in libraries/dph/ghc.mk, but
#           libraries/dph is not itself a package.
#      else
#	  add P to the list of packages

define extra-packages

# Collects some dirs containing ghc.mk files that we need to include:
BUILD_DIRS_EXTRA=

$$(foreach p,$$(patsubst libraries/%,%,$$(wildcard $$(shell grep '^[^ #][^ ]* \+\(dph\|extra\) \+[^ ]\+ \+[^ ]\+$$$$' packages | sed 's/ .*//'))),\
    $$(if $$(wildcard libraries/$$p/ghc-packages),\
        $$(eval BUILD_DIRS_EXTRA += libraries/$$p) \
        $$(foreach q,$$(shell cat libraries/$$p/ghc-packages2),$$(eval $$(call extra-package,$$p,$$p/$$q))),\
        $$(eval $$(call extra-package,$$p,$$p)))\
)
endef

define extra-package # $1 = package root, $2 = package
$(call trace, extra-package($1,$2))

EXTRA_PACKAGES += $2
$$(eval $$(call addPackage,$2))

endef
