TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: T4007
T4007:
	$(RM) -f T4007.hi T4007.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O -ddump-rule-firings T4007.hs

