TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: ghci.prog022a
ghci.prog022a:
	'$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) $(WAY_FLAGS) $(ghciWayFlags) -v0 -fno-load-initial-targets -i -i. A B < ghci.prog022a.script

.PHONY: ghci.prog022b
ghci.prog022b:
	'$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) $(WAY_FLAGS) $(ghciWayFlags) -v0 -fno-load-initial-targets -i -i. A B < ghci.prog022b.script

.PHONY: ghci.prog022c
ghci.prog022c:
	'$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) $(WAY_FLAGS) $(ghciWayFlags) -v0 -fimport-loaded-targets -i -i. A B < ghci.prog022c.script

.PHONY: ghci.prog022d
ghci.prog022d:
	'$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) $(WAY_FLAGS) $(ghciWayFlags) -v0 -fno-load-initial-targets -fimport-loaded-targets -i -i. A B < ghci.prog022d.script

.PHONY: ghci.prog022e
ghci.prog022e:
	'$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) $(WAY_FLAGS) $(ghciWayFlags) -v0 -fimport-loaded-targets -i -i. A B < ghci.prog022e.script

.PHONY: ghci.prog022f
ghci.prog022f:
	'$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) $(WAY_FLAGS) $(ghciWayFlags) -v0  -fno-load-initial-targets -fimport-loaded-targets -i -i. A B < ghci.prog022f.script
