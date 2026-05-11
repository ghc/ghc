TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: prog-mhu006a
prog-mhu006a:
	'$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) $(WAY_FLAGS) $(ghciWayFlags) \
		-no-user-package-db \
		-unit @unitA -unit @unitB < prog-mhu006a.script
