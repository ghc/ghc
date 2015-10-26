TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

checkExists = [ -f $1 ] || echo $1 missing

.PHONY: dynamicToo005
# Check that "-c -dynamic-too" works with signatures
dynamicToo005:
	"$(TEST_HC)" $(TEST_HC_OPTS) -dynamic-too -v0 \
                               -sig-of A005=base:Prelude \
                               -c A005.hs-boot
	$(call checkExists,A005.o-boot)
	$(call checkExists,A005.hi-boot)
	"$(TEST_HC)" $(TEST_HC_OPTS) -dynamic-too -v0 \
                               --merge-requirements A005
	$(call checkExists,A005.o)
	$(call checkExists,A005.hi)
	$(call checkExists,A005.dyn_o)
	$(call checkExists,A005.dyn_hi)
