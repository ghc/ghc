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


# Add files to the bindist. Invoke like this:
#
# $(eval $(call bindist,utils/genapply,ghc.mk))

define bindist
# $1 = name
# $2 = files

.PHONY: bindist_$1
bindist: bindist_$1

bindist_$1:
	$(foreach i,$2,\
	    $(call make-command,\
	        for f in $i; do echo $(BIN_DIST_NAME)/$$$$f >> bindist-list; done \
	    ) \
	    )
endef

