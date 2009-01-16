TOP = ..

# If we're cleaning then we don't want to do all the GHC detection hardwork,
# and we certainly don't want to fail if GHC etc can't be found!
# However, we can't just put this conditional in boilerplate.mk, as
# some of the tests have a "clean" makefile target that relies on GHC_PKG
# being defined.
ifneq "$(MAKECMDGOALS)" "clean"
ifneq "$(MAKECMDGOALS)" "distclean"

include $(TOP)/mk/boilerplate.mk

TIMEOUT_PROGRAM = install-inplace/bin/timeout$(exeext)

PREFIX := $(abspath install-inplace)
$(eval $(call canonicalise,PREFIX))

ifneq "$(GCC)" ""
WITH_GCC = --with-gcc=$(GCC)
endif

ifeq "$(filter thr, $(GhcRTSWays))" ""
$(TIMEOUT_PROGRAM): timeout.py
	rm -rf install-inplace
	mkdir install-inplace
	mkdir install-inplace/bin
	cp $< $@
	chmod +x $@
else
$(TIMEOUT_PROGRAM): timeout.hs
	rm -rf install-inplace
	$(TEST_HC) --make Setup
	./Setup configure --with-compiler=$(TEST_HC) \
	                  --with-hc-pkg=$(GHC_PKG) \
	                  $(WITH_GCC) \
	                  --ghc-option=-threaded --prefix=$(PREFIX)
	./Setup build
	./Setup install
endif

boot all :: calibrate.out $(TIMEOUT_PROGRAM)

calibrate.out:
	$(RM) -f TimeMe.o TimeMe.hi TimeMe TimeMe.exe
	$(PYTHON) calibrate "$(GHC_STAGE1)" > $@
# We use stage 1 to do the calibration, as stage 2 may not exist.
# This isn't necessarily the compiler we'll be running the testsuite
# with, but it's really the performance of the machine that we're
# interested in

endif
endif

clean distclean:
	-./Setup clean
	$(RM) -rf install-inplace
	$(RM) -f calibrate.out
	$(RM) -f Setup Setup.exe Setup.hi Setup.o

