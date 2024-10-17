TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -f *.o *.hi

T16234:
	$(MAKE) -s --no-print-directory clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -c DataPeano.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c ControlMonadPrimitive.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c ControlMonadClassesCore.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c ControlMonadClassesEffects.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c ControlMonadClassesReader.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c ControlMonadClassesState.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c ControlMonadClasses.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Main.hs
