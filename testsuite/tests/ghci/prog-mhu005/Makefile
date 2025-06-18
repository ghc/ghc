TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: prog-mhu005a
prog-mhu005a:
	'$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) $(WAY_FLAGS) $(ghciWayFlags) -v0 -fno-load-initial-targets -unit @unitA -unit @unitB < prog-mhu005a.script
