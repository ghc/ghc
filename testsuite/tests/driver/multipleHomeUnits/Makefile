TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

EXECUTABLE_C = c/C$(exeext)
EXECUTABLE_D = d/D$(exeext)
CLEAN_FILES = a/A.o a/A.hi a/A.hie b/B.o b/B.hi b/B.hie c/C.o c/C.hi c/C.hie d/C.o d/C.hi d/C.hie $(EXECUTABLE_C) $(EXECUTABLE_D) essentials-home/GHC/Essentials.o essentials-home/GHC/Essentials.hi essentials-user/M.o essentials-user/M.hi essentials-order-base/B.o essentials-order-base/B.hi essentials-order-user/U.o essentials-order-user/U.hi

clean:
	$(RM) $(CLEAN_FILES)

multipleHomeUnits_callstack: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -unit @unitCallstack -v0
	! ./callstack/Main

multipleHomeUnits002: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -fhide-source-paths -unit @unitC -unit @unitD
	./$(EXECUTABLE_C)
	./$(EXECUTABLE_D)

multipleHomeUnits003: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -fhide-source-paths -unit @unitA -unit @unitB -unit @unitC -unit @unitD
	./$(EXECUTABLE_C)
	./$(EXECUTABLE_D)

multipleHomeUnits004_recomp: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -fhide-source-paths -unit @unitB -unit @unitE
	'$(TEST_HC)' $(TEST_HC_OPTS) -fhide-source-paths -unit @unitB -unit @unitE

multipleHomeUnitsModuleVisibility: clean
	! '$(TEST_HC)' $(TEST_HC_OPTS) -fhide-source-paths -unit @unitMV -unit @unitMV-import

multipleHomeUnits_recomp: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -fhide-source-paths -unit @unitRecomp -unit @unitDep
	# Doesn't cause recomp when TH is not involved
	echo "recomp=()" >> Dep.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -fhide-source-paths -unit @unitRecomp -unit @unitDep

multipleHomeUnits_essentials: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -fhide-source-paths -unit @unitEssentialsHome -unit @unitEssentialsUser
	# Nothing has changed, so the second run must not recompile anything.
	'$(TEST_HC)' $(TEST_HC_OPTS) -fhide-source-paths -unit @unitEssentialsHome -unit @unitEssentialsUser

multipleHomeUnits_essentials_recomp: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -fhide-source-paths -unit @unitEssentialsHome -unit @unitEssentialsUser
	# Check recompilation is triggered when the export list of GHC.Essentials changes.
	echo "newExport :: forall a. a -> a" >> essentials-home/GHC/Essentials.hs
	echo "newExport x = x" >> essentials-home/GHC/Essentials.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -fhide-source-paths -unit @unitEssentialsHome -unit @unitEssentialsUser
