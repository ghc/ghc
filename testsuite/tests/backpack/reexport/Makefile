TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Testing recompilation for backpack
bkpreex03:
	"$(TEST_HC)" $(TEST_HC_OPTS) -v1 --backpack bkpreex03.bkp -fhide-source-paths
	sed -i 's/import M1/import M2/' bkpreex03.bkp
	"$(TEST_HC)" $(TEST_HC_OPTS) -v1 --backpack bkpreex03.bkp -fhide-source-paths

bkpreex04:
	"$(TEST_HC)" $(TEST_HC_OPTS) -v1 --backpack bkpreex04.bkp -fhide-source-paths
	cp bkpreex04a.bkp bkpreex04.bkp
	"$(TEST_HC)" $(TEST_HC_OPTS) -v1 --backpack bkpreex04.bkp -fhide-source-paths
