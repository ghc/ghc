TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T7116:
	$(RM) -f T7116.o T7116.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -O2 -c -ddump-simpl T7116.hs
