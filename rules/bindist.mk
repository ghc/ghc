
# Add files to the bindist. Invoke like this:
#
# $(eval $(call bindist,utils/genapply,ghc.mk))

define bindist
# $1 = dir
# $2 = files

.PHONY: bindist_$1
bindist: bindist_$1

bindist_$1:
	for f in $$(addprefix $1/,$2); do echo $(BIN_DIST_NAME)/$$$$f >> $(BIN_DIST_LIST); done
endef

