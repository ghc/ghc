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

docs/users_guide_DOCBOOK_SOURCES := \
    $(wildcard docs/users_guide/*.xml) \
    $(basename $(wildcard docs/users_guide/*.xml.in))

$(eval $(call docbook,docs/users_guide,users_guide))

