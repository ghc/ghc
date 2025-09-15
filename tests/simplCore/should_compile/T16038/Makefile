TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T16038:
	'$(TEST_HC)' $(TEST_HC_OPTS) -O -c A.hs-boot
	'$(TEST_HC)' $(TEST_HC_OPTS) -O -c B.hs
	# All `fEqHsExpr` bindings should be in one recursive group:
	'$(TEST_HC)' $(TEST_HC_OPTS) -O -c A.hs -ddump-simpl -dsuppress-all -fno-expose-overloaded-unfoldings | \
		grep -e "^\$$fEqHsExpr" -e "Rec"
