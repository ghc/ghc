TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: ghci.prog022a ghci.prog022b
ghci.prog022a:
	'$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) $(WAY_FLAGS) $(ghciWayFlags) -v0 -fno-load-initial-targets -i -i. A B < ghci.prog022a.script

ghci.prog022b:
	'$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) $(WAY_FLAGS) $(ghciWayFlags) -v0 -fno-load-initial-targets -i -i. A B < ghci.prog022b.script
