TIMEOUT_PROGRAM = $(INPLACE_BIN)/timeout$(exeext)

ifeq "$(findstring thr,$(GhcRTSWays))" ""
$(TIMEOUT_PROGRAM): testsuite/timeout/timeout.py
	$(MKDIRHIER) `dirname $@`
	cp $< $@
	chmod +x $@
else
testsuite/timeout_dist_MODULES = Main WinCBindings
testsuite/timeout_dist_PROG    = timeout
testsuite/timeout_dist_DEP_INCLUDE_DIRS = $(GHC_INCLUDE_DIR)
testsuite/timeout_HC_OPTS      = -threaded -XCPP -package process
ifeq "$(Windows)" "YES"
testsuite/timeout_HC_OPTS += -package Win32
else
testsuite/timeout_HC_OPTS += -package unix
endif

# XXX when GHC generates dependencies it uses the module name, not the
# filename, so we get dependencies on Main.o rather than timeout.o.
# If we don't fix this, timeout gets compiled before the libraries.
testsuite/timeout/dist/build/Main.hs : testsuite/timeout/timeout.hs $(MKDIRHIER)
	@$(MKDIRHIER) $(dir $@)
	$(CP) $< $@

$(eval $(call build-prog,testsuite/timeout,dist,1))
endif

all : testsuite/timeout/calibrate.out

# depend on $(GHC_STAGE2) so we can be sure all the libs are built
testsuite/timeout/calibrate.out: $(GHC_STAGE2)
	$(RM) -f testsuite/timeout/TimeMe.o testsuite/timeout/TimeMe.hi testsuite/timeout/TimeMe testsuite/timeout/TimeMe.exe
	cd testsuite/timeout && $(PYTHON) calibrate "$(GHC_STAGE1_ABS)" > ../../$@

CLEAN_FILES += testsuite/timeout/calibrate.out

# We use stage 1 to do the calibration, as stage 2 may not exist.
# This isn't necessarily the compiler we'll be running the testsuite
# with, but it's really the performance of the machine that we're
# interested in
