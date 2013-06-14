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


define manual-package-config # args: $1 = dir
$(call trace, manual-package-config($1))
$(call profStart, manual-package-config($1))

$1/dist/package.conf.inplace : $1/package.conf.in $$$$(ghc-pkg_INPLACE) | $$$$(dir $$$$@)/.
	$$(CPP) $$(RAWCPP_FLAGS) -P \
		-DTOP='"$$(TOP)"' \
		$$($1_PACKAGE_CPP_OPTS) \
		-x c $$(addprefix -I,$$(GHC_INCLUDE_DIRS)) $$< -o $$@.raw
	grep -v '^#pragma GCC' $$@.raw | \
	    sed -e 's/""//g' -e 's/:[ 	]*,/: /g' > $$@

	"$$(ghc-pkg_INPLACE)" update --force $$@

# This is actually a real file, but we need to recreate it on every
# "make install", so we declare it as phony
.PHONY: $1/dist/package.conf.install
$1/dist/package.conf.install: | $$$$(dir $$$$@)/.
	$$(CPP) $$(RAWCPP_FLAGS) -P \
		-DINSTALLING \
		-DLIB_DIR='"$$(if $$(filter YES,$$(RelocatableBuild)),$$$$topdir,$$(ghclibdir))"' \
		-DINCLUDE_DIR='"$$(if $$(filter YES,$$(RelocatableBuild)),$$$$topdir,$$(ghclibdir))/include"' \
		$$($1_PACKAGE_CPP_OPTS) \
		-x c $$(addprefix -I,$$(GHC_INCLUDE_DIRS)) $1/package.conf.in -o $$@.raw
	grep -v '^#pragma GCC' $$@.raw | \
	    sed -e 's/""//g' -e 's/:[ 	]*,/: /g' >$$@

$(call profEnd, manual-package-config($1))
endef
