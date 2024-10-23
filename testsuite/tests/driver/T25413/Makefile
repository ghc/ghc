TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T25413:
	echo 'int test(void) { return 114514; }' > dep.c
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghcThWayFlags) --make A.hs
	echo 'int test(void) { return 1919810; }' > dep.c
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghcThWayFlags) --make A.hs
