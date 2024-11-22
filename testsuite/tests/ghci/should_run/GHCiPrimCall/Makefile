TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: GHCiPrimCall
GHCiPrimCall:
	'$(TEST_HC)' $(TEST_HC_OPTS) -fPIC -v0 -c GHCiPrimCall_cmm.cmm
	-'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -e main GHCiPrimCall.hs GHCiPrimCall_cmm.o || echo $$? >&2
