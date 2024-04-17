TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: T4007 T16473
T4007:
	$(RM) -f T4007.hi T4007.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O -ddump-rule-firings T4007.hs

T11068:
	$(RM) -f T11068a.hi T11068a.o T11068b.hi T11068b.o T11068.hi T11068.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O T11068a.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O T11068b.hs
	-'$(TEST_HC)' $(TEST_HC_OPTS) -c -O T11068.hs -ddump-simpl | grep 'Generic'

MultiModulesRecomp:
	./genMultiLayerModules
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 MultiLayerModules.hs

# -e "" exits the ghci session immediately and merely makes sure, we generated interface files
# containing core expressions, aka `mi_extra_decls` are populated.
MultiModulesRecompDefsWithCore:
	./genMultiLayerModulesCore
	'$(TEST_HC)' --interactive $(TEST_HC_OPTS) -e "" -fwrite-if-simplified-core MultiLayerModules

MultiComponentModulesRecomp:
	'$(PYTHON)' genMultiComp.py
	TEST_HC='$(TEST_HC)' TEST_HC_OPTS='$(TEST_HC_OPTS)' ./run

MultiLayerModulesTH_Make_Prep:
	./genMultiLayerModulesTH
	"$(TEST_HC)" $(TEST_HC_OPTS) MultiLayerModulesPrep -dynamic-too -v0

MultiLayerModulesTH_OneShot_Prep: MultiLayerModulesTH_Make_Prep
	$(CP) MultiLayerModules.hs MultiLayerModulesTH_OneShot.hs

# Type family skolems
InstanceMatching:
	./genMatchingTest 0
	'$(TEST_HC)' $(TEST_HC_OPTS) -fno-code -fwrite-interface Defs.hs

T22744:
	./genT22744
	'$(TEST_HC)' $(TEST_HC_OPTS) T22744.hs

