TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

DECIMAL_REGEXP = [0-9]\+.[0-9]\+

.PHONY: T11489
T11489:
	$(RM) T11489
	touch T11489.prof
	chmod -w T11489.prof
	"$(TEST_HC)" -v0 -prof T11489.hs
	# Should print some message about not being to open the .prof file,
	# then continue to run and exit normally.
	# Caused a segmentation fault in GHC <= 7.10.3
	./T11489 +RTS -hr{} -hc

.PHONY: T14257
T14257:
	$(RM) T14257
	"$(TEST_HC)" -O2 --make -prof -v0 -fprof-auto T14257.hs
	./T14257 +RTS -hc
	# Make sure that samples are monotonically increasing
	awk 'BEGIN{t=0} /BEGIN_SAMPLE/{if ($$2 < t) print "uh oh", $$t, $$0; t=$$2;}' T14257.hp

.PHONY: T15897
T15897:
	# The bug is caught by an assertion so we run the tests with debug runtime
	# and ignore the outputs
	"$(TEST_HC)" -prof -fprof-auto -debug -v0 T15897.hs
	./T15897 10000000 +RTS -s -hc 2>/dev/null
	./T15897 10000000 +RTS -s -hr 2>/dev/null
