TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

define runT3736
./T3736 $1 +RTS -t --machine-readable 2>&1 | grep '"bytes allocated"' | sed -e 's/.*, "//' -e 's/".*//'
endef
.PHONY: T3736
T3736:
	$(RM) -f T3736.hi T3736.o T3736
	'$(TEST_HC)' -v0 -O --make T3736 -rtsopts
# Check ALLOC1 is > 100 just to check with have sane results, and if so,
# the test passes if the two numbers are equal. We could check that the
# actual numbers are in the range we expect too (on the various
# platforms), but we don't currently.
	ALLOC1=`$(call runT3736,1)`; ALLOC2=`$(call runT3736,2)`; if [ "$$ALLOC1" -gt 100 ] && [ "$$ALLOC1" -eq "$$ALLOC2" ]; then echo Match; else echo "Mismatch: $$ALLOC1 $$ALLOC2"; fi

