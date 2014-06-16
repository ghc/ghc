# -----------------------------------------------------------------------------
#
# (c) 2012 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------

define cross-compiling # $1 = then, $2 = else, $3 = then, ...
ifneq "$(TARGETPLATFORM)" "$(HOSTPLATFORM)"
ifneq "$(BUILDPLATFORM)" "$(HOSTPLATFORM)"
$(warning When cross-compiling, the build and host platforms must be equal (--build=$(BUILDPLATFORM) --host=$(HOSTPLATFORM) --target=$(TARGETPLATFORM)))
endif
$1
$3
else
$2
$4
endif
endef
