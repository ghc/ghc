
.PHONY: utils/testremove_all
utils/testremove_all: utils/testremove/wouldrm utils/testremove/checkremove

utils/testremove/wouldrm: $$@.hs
	$(GHC_STAGE1) --make -O $@

utils/testremove/checkremove: $$@.hs
	$(GHC_STAGE1) --make -O $@
