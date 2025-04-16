TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: ghci.prog020
ghci.prog020:
	'$(TEST_HC)' $(TEST_HC_OPTS) $(WAY_FLAGS) $(ghciWayFlags) -v0 A.hs B.hs
	'$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) $(WAY_FLAGS) $(ghciWayFlags) -v0 -i -i. A B < ghci.prog020.script
