$(MKDIRHIER) : utils/mkdirhier/mkdirhier.sh
	-mkdir $(INPLACE)
	-mkdir $(INPLACE_BIN)
	-mkdir $(INPLACE_LIB)
	$(RM) -f $@
	echo '#!$(SHELL)'  		 >> $@
	cat utils/mkdirhier/mkdirhier.sh >> $@
	$(EXECUTABLE_FILE) $@

$(eval $(call all-target,utils/mkdirhier,$(MKDIRHIER)))
$(eval $(call clean-target,utils/mkdirhier,,$(MKDIRHIER)))
