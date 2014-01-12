TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: hsc2hs001
hsc2hs001:
	'$(HSC2HS)' $@.hsc
	'$(TEST_HC)' $(TEST_HC_OPTS) -c $@.hs

.PHONY: hsc2hs002
hsc2hs002:
	'$(HSC2HS)' $@.hsc
	'$(TEST_HC)' $(TEST_HC_OPTS) -c $@.hs

.PHONY: hsc2hs003
hsc2hs003:
	'$(HSC2HS)' $@.hsc
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make $@
	./$@

.PHONY: hsc2hs004
hsc2hs004:
	'$(HSC2HS)' --cross-compile $@.hsc
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make $@
	./$@

.PHONY: T3837
T3837:
	LANG=C '$(HSC2HS)' $@.hsc
	'$(TEST_HC)' $(TEST_HC_OPTS) -c $@.hs
