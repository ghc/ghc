# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------

# If the user says 'make' or 'make stage=2' here, we behave as if they were
# in the ghc directory instead, so that the executable GHC gets built.
.PHONY: default_to_ghc all_ghc
default_to_ghc : all_ghc

dir = compiler

include ../mk/compiler-ghc.mk

all_ghc :
	+$(TOPMAKE) all_ghc $(EXTRA_MAKE_OPTS)

