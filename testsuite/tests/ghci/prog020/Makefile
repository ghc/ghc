TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: ghci.prog020a
ghci.prog020a:
	'$(TEST_HC)' $(TEST_HC_OPTS) $(WAY_FLAGS) $(ghciWayFlags) -v0 A.hs B.hs
	'$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) $(WAY_FLAGS) $(ghciWayFlags) -v0 -i -i. A B < ghci.prog020a.script

.PHONY: ghci.prog020b
ghci.prog020b:
	'$(TEST_HC)' $(TEST_HC_OPTS) $(WAY_FLAGS) $(ghciWayFlags) -v0 A.hs B.hs
	'$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) $(WAY_FLAGS) $(ghciWayFlags) -v0 -i -i. A B < ghci.prog020b.script
