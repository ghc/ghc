TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: TH_QQ
TH_QQ:
ifeq "$(GhcDynamic)" "YES"
	'$(TEST_HC)' $(TEST_HC_OPTS) -c QQ.hs -dynamic -osuf dyn_o -hisuf dyn_hi
else
	'$(TEST_HC)' $(TEST_HC_OPTS) -c QQ.hs
endif

