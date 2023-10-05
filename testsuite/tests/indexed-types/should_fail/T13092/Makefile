TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T13092:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c A.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c B.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c C.hs
	! '$(TEST_HC)' $(TEST_HC_OPTS) -c Main.hs
