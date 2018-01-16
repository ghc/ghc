
.PHONY : examples
examples :
	cd dph-examples ; \
	 $(GHC_DPH) --make Setup.hs ; \
	 ./Setup configure ; \
	 ./Setup build
