TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T7354a:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T7354b.hs
	-'$(TEST_HC)' $(TEST_HC_OPTS) -c T7354a.hs

T8227:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T8227a.hs
	-'$(TEST_HC)' $(TEST_HC_OPTS) -c T8227.hs

# T8129 is trying to ensure that we don't get an 
# an asertion failure with -ddump-tc-trace
T8129:
	-'$(TEST_HC)' $(TEST_HC_OPTS) -c -ddump-tc-trace T8129.hs 2> T8129.trace
	grep deduce T8129.trace

