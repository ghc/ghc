TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: T25240
T25240:
	"$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) T25240a.hs T25240.hs +RTS --optimistic-linking -RTS
