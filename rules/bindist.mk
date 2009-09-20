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
# $1 = name
# $2 = files

.PHONY: bindist_$1
bindist: bindist_$1

bindist_$1:
$(foreach i,$2,$(call bindist_item,$i))
endef

define bindist_item

# $1 = the line
# The formatting of this definition (e.g. the blank line above) is
# important, in order to get make to generate the right makefile code.
	for f in $1; do echo $(BIN_DIST_NAME)/$$$$f >> $(BIN_DIST_LIST); done
endef

