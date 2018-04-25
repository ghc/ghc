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

TOP = ../..
include $(TOP)/mk/boilerplate.mk

SRC_HC_OPTS += -package parallel -package parsec -fvia-C -fexcess-precision

include $(TOP)/mk/target.mk
