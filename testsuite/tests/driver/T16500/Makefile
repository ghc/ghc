TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T16500:
	$(RM) -rf interfaces objects
	$(RM) A.hi
	mkdir -p interfaces
	mkdir -p objects
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -c A.hs -iinterfaces -hidir interfaces -odir objects
	touch A.hi
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -c B.hs -iinterfaces -hidir interfaces -odir objects
