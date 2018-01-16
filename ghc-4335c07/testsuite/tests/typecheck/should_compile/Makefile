TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T14434:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T14434.hs -ddump-simpl | grep toStringX
         # Expecting toStringX = toString, not discarding argument

tc170:
	$(RM) Tc170_Aux.hi Tc170_Aux.o tc170.hi tc170.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Tc170_Aux.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c tc170.hs

tc173:
	$(RM) Tc173a.o Tc173a.hi Tc173b.o Tc173b.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Tc173a.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Tc173b.hs

T2412:
	$(RM) -f T2412.hi-boot T2412.o-boot T2412A.hi T2412A.o T2412.hi T2412.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T2412.hs-boot
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T2412A.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T2412.hs

tc245:
	$(RM) -f Tc245_A.hi Tc245_A.o tc245.hi tc245.o
	'$(TEST_HC)' $(TEST_HC_OPTS) --make tc245
	$(RM) -f tc245.hi tc245.o
	'$(TEST_HC)' $(TEST_HC_OPTS) --make tc245

# Trac #5792 gave an error on the second compilation,
# presumably because of the .hi file
T5792:
	$(RM) -f T5792.o T5792.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T5792.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T5792.hs -fforce-recomp

T7171:
	$(RM) -f T7171.hi-boot T7171.o-boot T7171a.hi T7171a.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T7171a.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T7171.hs

# I got this one from a infinite loop during validate
tc266:
	$(RM) -f Tc266.hi-boot Tc266.o-boot Tc266a.hi Tc266a.o Tc266.hi Tc266.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Tc266.hs-boot
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Tc266a.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Tc266.hs

Tc267:
	$(RM) -f Tc267a.hi-boot Tc267a.o-boot Tc267b.hi-boot Tc267b.o-boot
	$(RM) -f Tc267a.hi Tc267a.o Tc267b.hi Tc267b.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Tc267a.hs-boot
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Tc267b.hs-boot
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Tc267a.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Tc267b.hs

Tc271:
	$(RM) -f Tc271.hi-boot Tc271.o-boot Tc271a.hi Tc271a.o Tc271.hi Tc271.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Tc271.hs-boot
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Tc271a.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Tc271.hs

T13585:
	$(RM) -f T13585a.o T13585a.hi T13585b.o T13585b.hi T13585.o T13585.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T13585a.hs -O
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T13585b.hs -O
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T13585.hs  -O
