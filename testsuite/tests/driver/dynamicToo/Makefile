TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

checkExists = [ -f $1 ] || echo $1 missing

.PHONY: dynamicToo003
# Check that "-c -dynamic-too" works
dynamicToo003:
	"$(TEST_HC)" $(TEST_HC_OPTS) -dynamic-too -v0 -c A003.hs
	$(call checkExists,A003.o)
	$(call checkExists,A003.hi)
	$(call checkExists,A003.dyn_o)
	$(call checkExists,A003.dyn_hi)

