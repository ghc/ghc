TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/cabal-flags.mk

TIMEOUT_PROGRAM = install-inplace/bin/timeout$(exeext)

ifeq "$(findstring thr,$(GhcRTSWays))" ""
$(TIMEOUT_PROGRAM): timeout.py
	$(MKDIRHIER) `dirname $@`
	cp $< $@
	chmod +x $@
else
$(TIMEOUT_PROGRAM): timeout.hs
	$(CABAL) configure $(USE_STAGE1_CONFIGURE_FLAGS)   \
	                   $(INPLACE_DIRS_CONFIGURE_FLAGS) \
	                   $(COMMON_CONFIGURE_FLAGS)       \
					   --ghc-option=-threaded
	$(CABAL) build
	$(CABAL) install
endif

boot all :: calibrate.out $(TIMEOUT_PROGRAM)

calibrate.out:
	$(RM) -f TimeMe.o TimeMe.hi TimeMe TimeMe.exe
	$(PYTHON) calibrate "$(GHC_STAGE1)" > $@
# We use stage 1 to do the calibration, as stage 2 may not exist.
# This isn't necessarily the compiler we'll be running the testsuite
# with, but it's really the performance of the machine that we're
# interested in

clean distclean:
	-$(CABAL) clean
	$(RM) -rf install-inplace
	$(RM) -f calibrate.out

