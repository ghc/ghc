TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Makefile for oneshot mode compilation tests

.PHONY: SI08_oneshot SI09_oneshot SI10_oneshot clean

# For SI08_oneshot (expected to fail)
SI08_oneshot:
	"$(TEST_HC)" $(TEST_HC_OPTS) -c ClassA.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) -c InstanceA.hs
	! "$(TEST_HC)" $(TEST_HC_OPTS) -c SI08.hs

# For SI09_oneshot (expected to succeed)
SI09_oneshot:
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcThWayFlags) -c ClassA.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcThWayFlags) -c InstanceA.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcThWayFlags) -c SI09.hs

# For SI10_oneshot (expected to succeed)
SI10_oneshot:
	"$(TEST_HC)" $(TEST_HC_OPTS) -c ClassA.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) -c InstanceA.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) -c SI10.hs

clean:
	rm -f *.o *.hi
