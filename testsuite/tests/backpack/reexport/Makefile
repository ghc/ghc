TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Testing recompilation for backpack
bkpreex03:
	"$(TEST_HC)" $(TEST_HC_OPTS) -v1 --backpack bkpreex03.bkp
	sed -i 's/import M1/import M2/' bkpreex03.bkp
	"$(TEST_HC)" $(TEST_HC_OPTS) -v1 --backpack bkpreex03.bkp
