TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

define runT3736
./T3736 $1 +RTS -t --machine-readable 2>&1 | grep '"bytes allocated"' | sed -e 's/.*, "//' -e 's/".*//'
endef
.PHONY: T3736
T3736:
	$(RM) -f T3736.hi T3736.o T3736
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -O --make T3736 -rtsopts
# Check ALLOC1 is > 100 just to check with have sane results, and if so,
# the test passes if the two numbers are equal. We could check that the
# actual numbers are in the range we expect too (on the various
# platforms), but we don't currently.
	ALLOC1=`$(call runT3736,1)`; ALLOC2=`$(call runT3736,2)`; if [ "$$ALLOC1" -gt 100 ] && [ "$$ALLOC1" -eq "$$ALLOC2" ]; then echo Match; else echo "Mismatch: $$ALLOC1 $$ALLOC2"; fi

.PHONY: T2902
T2902:
	$(RM) -f T2902_A    T2902_B
	$(RM) -f T2902_A.hi T2902_B.hi
	$(RM) -f T2902_A.o  T2902_B.o
	$(RM) -f T2902_A_PairingSum.hi T2902_B_PairingSum.hi T2902_Sum.hi
	$(RM) -f T2902_A_PairingSum.o  T2902_B_PairingSum.o  T2902_Sum.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -O --make T2902_A -rtsopts
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -O --make T2902_B -rtsopts
	BAA=`./T2902_A +RTS -t --machine-readable 2>&1 | grep '"bytes allocated"' | sed -e 's/.*, "//' -e 's/")//'`; BAB=`./T2902_B +RTS -t --machine-readable 2>&1 | grep '"bytes allocated"' | sed -e 's/.*, "//' -e 's/")//'`; [ "$$BAA" = "" ] && echo 'T2902_A: No "bytes allocated"'; [ "$$BAA" = "$$BAB" ] || echo "T2902: Mismatch in \"bytes allocated\": $$BAA $$BAB"

.PHONY: T149
T149:
	$(RM) -f T149_A T149_A.hi T149_A.o
	$(RM) -f T149_B T149_B.hi T149_B.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -O --make T149_A -rtsopts
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -O --make T149_B -rtsopts
	BAA=`./T149_A +RTS -t --machine-readable 2>&1 | grep '"bytes allocated"' | sed -e 's/.*, "//' -e 's/")//'`; BAB=`./T149_B +RTS -t --machine-readable 2>&1 | grep '"bytes allocated"' | sed -e 's/.*, "//' -e 's/")//'`; [ "$$BAA" = "" ] && echo 'T149_A: No "bytes allocated"'; [ "$$BAA" = "$$BAB" ] || echo "T149: Mismatch in \"bytes allocated\": $$BAA $$BAB"

