TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

checkExists = [ -f $1 ] || echo $1 missing

.PHONY: dynamicToo005
# Check that "-c -dynamic-too" works with .hsig
dynamicToo005:
	"$(TEST_HC)" $(TEST_HC_OPTS) --backpack dynamicToo005.bkp -dynamic-too -v0
	$(call checkExists,sig/A005.hi)
	$(call checkExists,sig/A005.dyn_hi)
	$(call checkExists,sig/sig-*/A005.o)
	$(call checkExists,sig/sig-*/A005.hi)
	$(call checkExists,sig/sig-*/A005.dyn_o)
	$(call checkExists,sig/sig-*/A005.dyn_hi)
