TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

TESTS = hsc2hs001 hsc2hs002

.PHONY: $(TESTS)

$(TESTS):
	'$(HSC2HS)' $@.hsc
	'$(TEST_HC)' -c $@.hs

