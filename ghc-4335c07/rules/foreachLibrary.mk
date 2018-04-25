# -----------------------------------------------------------------------------
#
# (c) 2010 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Modifying
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

# We need to handle the following packages specially, as those don't
# have an entry in the packages file, since they don't live in
# repositories of their own:
#
#  - base
#  - ghc-boot
#  - ghc-boot-th
#  - ghc-prim
#  - integer-gmp
#  - integer-simple
#  - template-haskell

define foreachLibrary
# $1 = function to call for each library
# We will give it the package path and the tag as arguments
$$(foreach hashline,libraries/ghc-boot-th#-#no-remote-repo#no-vcs           \
                    libraries/ghc-boot#-#no-remote-repo#no-vcs              \
                    libraries/ghci#-#no-remote-repo#no-vcs                  \
                    libraries/base#-#no-remote-repo#no-vcs                  \
                    libraries/ghc-prim#-#no-remote-repo#no-vcs              \
                    libraries/integer-gmp#-#no-remote-repo#no-vcs           \
                    libraries/integer-simple#-#no-remote-repo#no-vcs        \
                    libraries/template-haskell#-#no-remote-repo#no-vcs      \
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
