
.PHONY: utils/testremove_all
utils/testremove_all: utils/testremove/wouldrm utils/testremove/checkremove

utils/testremove/wouldrm: $$@.hs
	$(HC_STAGE1) --make -O $@

utils/testremove/checkremove: $$@.hs
	$(HC_STAGE1) --make -O $@
