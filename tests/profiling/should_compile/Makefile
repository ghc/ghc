TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T14931:
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -c -O -static -dynamic-too -dynosuf dyn_o -dynhisuf dyn_hi T14931_State.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -c -O -prof -osuf p_o -hisuf p_hi T14931_State.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -c -O -prof -osuf p_o -hisuf p_hi T14931_Bug.hs
