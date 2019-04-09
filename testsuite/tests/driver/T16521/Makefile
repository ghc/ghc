TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T16521 :
	rm -f Makefile.out
	'$(TEST_HC)' $(TEST_HC_OPTS) -package process -M -include-cpp-deps -dep-suffix "" -dep-makefile Makefile1.out A.hs > /dev/null
	'$(TEST_HC)' $(TEST_HC_OPTS) -package process -M -include-cpp-deps -dep-suffix "" -dep-suffix "_" -dep-makefile Makefile2.out A.hs > /dev/null
	./check.sh