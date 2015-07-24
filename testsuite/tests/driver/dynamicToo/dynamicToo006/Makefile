TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

checkExists = [ -f $1 ] || echo $1 missing

.PHONY: dynamicToo006
# Check that "--make -dynamic-too" works with .hsig
dynamicToo006:
	"$(TEST_HC)" $(TEST_HC_OPTS) -dynamic-too -v0 \
                               -sig-of A=base:Prelude \
                               --make B
	$(call checkExists,A.o)
	$(call checkExists,A.o-boot)
	$(call checkExists,B.o)
	$(call checkExists,A.hi)
	$(call checkExists,A.hi-boot)
	$(call checkExists,B.hi)
	$(call checkExists,A.dyn_o)
	$(call checkExists,B.dyn_o)
	$(call checkExists,A.dyn_hi)
	$(call checkExists,B.dyn_hi)
