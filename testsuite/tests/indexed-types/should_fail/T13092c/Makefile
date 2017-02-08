TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T13092c:
	rm -f T13092c_[1234].hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T13092c_1.hs -ddump-if-trace > /dev/null 2> if.out
	! grep 'Reading interface for .*:GHC.Generics' if.out
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T13092c_2.hs -ddump-if-trace > /dev/null 2> if.out
	! grep 'Reading interface for .*:GHC.Generics' if.out
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T13092c_3.hs
	! '$(TEST_HC)' $(TEST_HC_OPTS) -c T13092c_4.hs
