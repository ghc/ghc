utils/hasktags_dist_MODULES = Main
utils/hasktags_dist_PROG    = hasktags$(exeext)
utils/hasktags_dist_INSTALL = YES

utils/hasktags/dist/build/Main.hs : utils/hasktags/HaskTags.hs $(MKDIRHIER)
	$(MKDIRHIER) $(dir $@)
	$(CP) $< $@

$(eval $(call build-prog,utils/hasktags,dist,1))

