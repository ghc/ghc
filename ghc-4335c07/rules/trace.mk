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

# A helpful little debug macro.  Call it from a macro like this:
# 
#  $(call trace, this-macro($1,$2,$3))
#
# And invoke the build system with TRACE=1 to turn on tracing.

define trace
$$(if $(TRACE),$$(warning $1),)
endef
