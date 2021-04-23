TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: T4007 T16473
T4007:
	$(RM) -f T4007.hi T4007.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O -ddump-rule-firings T4007.hs

T11068:
	$(RM) -f T11068a.hi T11068a.o T11068b.hi T11068b.o T11068.hi T11068.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O T11068a.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O T11068b.hs
	-'$(TEST_HC)' $(TEST_HC_OPTS) -c -O T11068.hs -ddump-simpl | grep 'Generic'

InstanceMatching:
	./genMatchingTest
	'$(TEST_HC)' $(TEST_HC_OPTS) -fno-code -fwrite-interface Defs.hs
