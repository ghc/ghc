TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

checkExists = [ -f $1 ] || echo $1 missing

.PHONY: dynamicToo005
# Check that "-c -dynamic-too" works with .hsig
dynamicToo005:
	"$(TEST_HC)" $(TEST_HC_OPTS) -dynamic-too -v0 \
                               -sig-of A005=base:Prelude \
                               -c A005.hsig
	$(call checkExists,A005.o)
	$(call checkExists,A005.hi)
	$(call checkExists,A005.dyn_o)
	$(call checkExists,A005.dyn_hi)
