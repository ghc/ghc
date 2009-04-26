$(MKDEPENDC) : utils/mkdependC/mkdependC.prl $(MKDIRHIER)
	$(MKDIRHIER) $(dir $@)
	$(RM) -f $@
	echo '#!$(PERL)'                               >> $@
	echo '$$DEFAULT_TMPDIR = "$(DEFAULT_TMPDIR)";' >> $@
	echo '$$CPP            = "$(CPP)";'            >> $@
	echo '$$BUILDPLATFORM  = "$(BUILDPLATFORM)";'  >> $@
	cat utils/mkdependC/mkdependC.prl              >> $@
	$(EXECUTABLE_FILE) $@

$(eval $(call all-target,utils/mkdependC,$(MKDEPENDC)))
$(eval $(call clean-target,utils/mkdependC,,$(MKDEPENDC)))
