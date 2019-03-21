# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      https://gitlab.haskell.org/ghc/ghc/wikis/building/architecture
#      https://gitlab.haskell.org/ghc/ghc/wikis/building/modifying
#
# -----------------------------------------------------------------------------


# Add files to the bindist-list. Invoke like this:
#
# $(eval $(call bindist-list,utils/genapply,ghc.mk))

define bindist-list
# $1 = name
# $2 = files

.PHONY: bindist-list_$1
bindist-list: bindist-list_$1

bindist-list_$1:
	$(foreach i,$2,\
	    $(call make-command,\
	        for f in $i; do echo $(BIN_DIST_NAME)/$$$$f >> bindist-list; done \
	    ) \
	    )
endef

