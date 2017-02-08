TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T13092b:
	rm -f T13092b_[12].hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T13092b_1.hs -ddump-if-trace 2> if.out
	grep 'Reading interface for .*:GHC.Generics' if.out > /dev/null
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T13092b_2.hs -ddump-if-trace 2> if.out
	! grep 'Reading interface for .*:GHC.Generics' if.out
