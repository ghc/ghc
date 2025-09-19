TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T26430_setup :
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghciWayFlags) -shared T26430A.c -o libT26430A.so
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghciWayFlags) -shared -L. -lT26430A T26430B.c -o libT26430B.so
