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

# For each line in $(TOP)/packages:
#     libraries/foo    tag    ...
# this calls
#     $(call $1,foo,tag)
#
# Except! If there's a libraries/foo/ghc-packages then it calls
#     $(call $1,foo/bar,tag)
# for each word 'bar' in libraries/foo/ghc-packages.
# 

# We use an FEL_ prefix for the variable names, to avoid trampling on
# other variables, as make has no concept of local variables.

# We need to handle bin-package-db specially, as it doesn't have an
# entry in the packages file, as it isn't in its own repository.

define foreachLibrary
# $1 = function to call for each library
# We will give it the package path and the tag as arguments
$$(foreach hashline,libraries/bin-package-db#-#no-remote-repo#no-vcs        \
                    $$(shell grep '^libraries/' packages | sed 's/  */#/g'),\
    $$(eval FEL_line    := $$(subst #,$$(space),$$(hashline)))              \
    $$(eval FEL_libdir  := $$(word 1,$$(FEL_line)))                         \
    $$(eval FEL_tag     := $$(word 2,$$(FEL_line)))                         \
    $$(eval FEL_libroot := $$(patsubst libraries/%,%,$$(FEL_libdir)))       \
    $$(if $$(wildcard $$(FEL_libdir)/ghc-packages),                         \
        $$(foreach lib,$$(shell cat $$(FEL_libdir)/ghc-packages),           \
            $$(eval $$(call $1,$$(FEL_libroot)/$$(lib),$$(FEL_tag)))),      \
        $$(if $$(wildcard $$(FEL_libdir)/),                                 \
            $$(eval $$(call $1,$$(FEL_libroot),$$(FEL_tag))))))
endef
