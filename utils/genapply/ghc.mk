utils/genapply_dist_MODULES = GenApply
utils/genapply_dist_PROG    = $(GHC_GENAPPLY_PGM)

ifeq "$(ghc_ge_607)" "YES"
utils/genapply_HC_OPTS += -package pretty
endif

ifeq "$(GhcUnregisterised)" "YES"
utils/genapply_HC_OPTS += -DNO_REGS
endif

utils/genapply/GenApply.hs : $(GHC_INCLUDE_DIR)/ghcconfig.h
utils/genapply/GenApply.hs : $(GHC_INCLUDE_DIR)/MachRegs.h
utils/genapply/GenApply.hs : $(GHC_INCLUDE_DIR)/Constants.h

$(eval $(call build-prog,utils/genapply,dist,0))
