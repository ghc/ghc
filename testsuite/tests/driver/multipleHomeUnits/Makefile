TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

EXECTUABLE_C = c/C$(exeext)
EXECTUABLE_D = d/D$(exeext)
CLEAN_FILES = a/A.o a/A.hi a/A.hie b/B.o b/B.hi b/B.hie c/C.o c/C.hi c/C.hie d/C.o d/C.hi d/C.hie $(EXECTUABLE_C) $(EXECTUABLE_D)

clean:
	$(RM) $(CLEAN_FILES)

multipleHomeUnits_callstack: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -unit @unitCallstack -v0
	! ./callstack/Main

multipleHomeUnits002: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -fhide-source-paths -unit @unitC -unit @unitD
	./$(EXECTUABLE_C)
	./$(EXECTUABLE_D)

multipleHomeUnits003: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -fhide-source-paths -unit @unitA -unit @unitB -unit @unitC -unit @unitD
	./$(EXECTUABLE_C)
	./$(EXECTUABLE_D)

multipleHomeUnits004_recomp: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -fhide-source-paths -unit @unitB -unit @unitE
	'$(TEST_HC)' $(TEST_HC_OPTS) -fhide-source-paths -unit @unitB -unit @unitE

multipleHomeUnitsModuleVisibility: clean
	! '$(TEST_HC)' $(TEST_HC_OPTS) -fhide-source-paths -unit @unitMV -unit @unitMV-import


