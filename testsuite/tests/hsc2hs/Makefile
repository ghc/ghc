TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

ALL_TESTS = hsc2hs001 hsc2hs002

.PHONY: $(TESTS)

$(ALL_TESTS):
	'$(HSC2HS)' $@.hsc
	'$(TEST_HC)' $(TEST_HC_OPTS) -c $@.hs

.PHONY: 3837
3837:
	LANG=C '$(HSC2HS)' $@.hsc
	'$(TEST_HC)' $(TEST_HC_OPTS) -c $@.hs
