TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

checkExists = [ -f $1 ] || echo $1 missing

.PHONY: dynamicToo002
# Check that "--make -dynamic-too" works
dynamicToo002:
	"$(TEST_HC)" $(TEST_HC_OPTS) -dynamic-too -v0 --make C
	$(call checkExists,A.o)
	$(call checkExists,B.o)
	$(call checkExists,C.o)
	$(call checkExists,A.hi)
	$(call checkExists,B.hi)
	$(call checkExists,C.hi)
	$(call checkExists,A.dyn_o)
	$(call checkExists,B.dyn_o)
	$(call checkExists,C.dyn_o)
	$(call checkExists,A.dyn_hi)
	$(call checkExists,B.dyn_hi)
	$(call checkExists,C.dyn_hi)

