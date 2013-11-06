TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

NewTyCo:
	$(RM) NewTyCo1.o NewTyCo1.hi NewTyCo2.o NewTyCo2.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -c NewTyCo1.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c NewTyCo2.hs

.PHONY: IndTypesPerf
IndTypesPerf:
	$(RM) IndTypesPerf.o IndTypesPerf.hi
	$(RM) IndTypesPerfMerge.o IndTypesPerfMerge.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -O -c IndTypesPerfMerge.hs +RTS -M20M
	'$(TEST_HC)' $(TEST_HC_OPTS) -O -c IndTypesPerf.hs      +RTS -M20M

T5955:
	$(RM) T5955.o T5955.hi T5955a.o T5955a.hi 
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T5955a.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T5955.hs


# Compiling twice made 7.4.1 break
T6152:
	$(RM) T6152.o T6152.hi 
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T6152.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T6152.hs -fforce-recomp

T8002:
	$(RM) T8002a.o T8002a.hi T8002b.o T8002b.hi 
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T8002b.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T8002a.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T8002.hs

T8011:
	$(RM) T8011a.o T8011a.hi T8011.o T8011.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T8011a.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T8011.hs

T8500:
	$(RM) T8500a.o T8500a.hi T8500.o T8500.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T8500a.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T8500.hs
