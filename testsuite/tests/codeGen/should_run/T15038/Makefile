TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: T15038
T15038:
	'$(TEST_HC)' $(TEST_HC_OPTS) $(shell find . -iname '*.hs') -O2 -o Main \
		-package containers -package ghc-prim -package primitive
	./Main
