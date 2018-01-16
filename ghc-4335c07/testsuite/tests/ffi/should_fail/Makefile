TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: UnsafeReenterGhci
UnsafeReenterGhci:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c UnsafeReenterC.c
	echo ':main' | '$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) UnsafeReenterC.o UnsafeReenter.hs
