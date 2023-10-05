TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

DRFPatSynExport:
	$(RM) DRFPatSynExport.hi DRFPatSynExport.o DRFPatSynExport.imports
	$(RM) DRFPatSynExport_A.hi DRFPatSynExport_A.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c DRFPatSynExport_A.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c DRFPatSynExport.hs -ddump-minimal-imports
	cat DRFPatSynExport.imports
