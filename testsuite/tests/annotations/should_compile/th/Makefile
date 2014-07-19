TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

annth_make:
	$(MAKE) clean_annth_make
	mkdir build_make
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make \
	   -odir build_make -hidir build_make -o build_make/annth annth.hs

clean_annth_make:
	rm -rf build_make

annth_compunits:
	$(MAKE) clean_annth_compunits
	mkdir build_compunits
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -dynamic-too \
	  -odir build_compunits -hidir build_compunits \
	  -c AnnHelper.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -dynamic-too \
	  -odir build_compunits -hidir build_compunits \
	  -c TestModule.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -dynamic-too \
	  -odir build_compunits -hidir build_compunits \
	  -c TestModuleTH.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -ibuild_compunits \
	  -odir build_compunits -hidir build_compunits \
	  -c annth.hs

clean_annth_compunits:
	rm -rf build_compunits

.PHONY: annth_make clean_annth_make annth_compunits clean_annth_compunits
