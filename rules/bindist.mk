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


# Add files to the bindist. Invoke like this:
#
# $(eval $(call bindist,utils/genapply,ghc.mk))

define bindist
# $1 = dir
# $2 = files

.PHONY: bindist_$1
bindist: bindist_$1

bindist_$1:
	for f in $2; do echo $1/$(BIN_DIST_NAME)/$$$$f >> $(BIN_DIST_LIST); done
endef

