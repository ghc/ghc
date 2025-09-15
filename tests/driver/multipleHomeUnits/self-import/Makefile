TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

multipleHomeUnits_self-import:
	'$(TEST_HC)' $(TEST_HC_OPTS) -unit @unitP1 -unit @unitP2 -v0
	# This should do nothing
	'$(TEST_HC)' $(TEST_HC_OPTS) -unit @unitP1 -unit @unitP2

