
$(eval $(call all-target,driver,$(INPLACE_LIB)/ghc-usage.txt) $(INPLACE_LIB)/ghci-usage.txt)

$(INPLACE_LIB)/ghc-usage.txt: driver/ghc-usage.txt
	cp $< $@

$(INPLACE_LIB)/ghci-usage.txt: driver/ghci-usage.txt
	cp $< $@

INSTALL_LIBS += driver/ghc-usage.txt driver/ghci-usage.txt

