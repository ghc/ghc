TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T5252:
	$(RM) -f T5252*.hi T5252*.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T5252a.hs 
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T5252.hs

# Failed when compiled *without* optimisation
T5252Take2:
	$(RM) -f T5252*.hi T5252*.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T5252Take2a.hs 
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T5252Take2.hs
