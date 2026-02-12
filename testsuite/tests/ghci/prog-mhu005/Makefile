TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: prog-mhu005a
prog-mhu005a:
	'$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) $(WAY_FLAGS) $(ghciWayFlags) -v0 -fno-load-initial-targets -unit @unitA -unit @unitB < prog-mhu005a.script

.PHONY: prog-mhu005b
prog-mhu005b:
	'$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) $(WAY_FLAGS) $(ghciWayFlags) -v0 -fimport-loaded-targets -unit @unitA -unit @unitB < prog-mhu005b.script

.PHONY: prog-mhu005c
prog-mhu005c:
	'$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) $(WAY_FLAGS) $(ghciWayFlags) -v0 -fno-import-loaded-targets -unit @unitA -unit @unitB < prog-mhu005c.script

.PHONY: prog-mhu005d
prog-mhu005d:
	'$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) $(WAY_FLAGS) $(ghciWayFlags) -v0 -fno-load-initial-targets -fno-import-loaded-targets -unit @unitA -unit @unitB < prog-mhu005d.script

.PHONY: prog-mhu005e
prog-mhu005e:
	'$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) $(WAY_FLAGS) $(ghciWayFlags) -v0 -fno-load-initial-targets -fimport-loaded-targets -unit @unitA -unit @unitB < prog-mhu005e.script

.PHONY: prog-mhu005f
prog-mhu005f:
	'$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) $(WAY_FLAGS) $(ghciWayFlags) -v0 -unit @unitA -unit @unitB < prog-mhu005f.script

.PHONY: prog-mhu005g
prog-mhu005g:
	'$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) $(WAY_FLAGS) $(ghciWayFlags) -v0 -fno-load-initial-targets -fimport-loaded-targets  -unit @unitA -unit @unitB < prog-mhu005g.script
