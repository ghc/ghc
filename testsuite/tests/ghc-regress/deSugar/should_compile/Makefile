TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T5252:
	$(RM) -f T5252*.hi T5252*.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T5252a.hs 
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T5252.hs
