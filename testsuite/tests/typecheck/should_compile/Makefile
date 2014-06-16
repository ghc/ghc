TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

tc170:
	$(RM) Tc170_Aux.hi Tc170_Aux.o tc170.hi tc170.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Tc170_Aux.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c tc170.hs

tc173:
	$(RM) Tc173a.o Tc173a.hi Tc173b.o Tc173b.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -XFlexibleInstances -XTypeSynonymInstances -XUndecidableInstances -XOverlappingInstances Tc173a.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -XUndecidableInstances -XOverlappingInstances Tc173b.hs

T2412:
	$(RM) -f T2412.hi-boot T2412.o-boot T2412A.hi T2412A.o T2412.hi T2412.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T2412.hs-boot
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T2412A.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T2412.hs

tc245:
	$(RM) -f Tc245_A.hi Tc245_A.o tc245.hi tc245.o
	'$(TEST_HC)' $(filter-out -fforce-recomp,$(TEST_HC_OPTS)) --make tc245
	$(RM) -f tc245.hi tc245.o
	'$(TEST_HC)' $(filter-out -fforce-recomp,$(TEST_HC_OPTS)) --make tc245

# Trac #5792 gave an error on the second compilation,
# presumably because of the .hi file
T5792:
	$(RM) -f T5792.o T5792.hi
	'$(TEST_HC)' -c T5792.hs
	'$(TEST_HC)' -c T5792.hs -fforce-recomp

T7171:
	$(RM) -f T7171.hi-boot T7171.o-boot T7171a.hi T7171a.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T7171a.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T7171.hs

