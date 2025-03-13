TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

RecompExportedDefault:
	rm -f *.o *.hi
	# -Wno-duplicate-exports due to #25857
	'$(TEST_HC)' $(TEST_HC_OPTS) -c A.hs -Wno-duplicate-exports
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompExportedDefault.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) RecompExportedDefault.o A.o -o RecompExportedDefault
	./RecompExportedDefault

# Test 1: Changing the defaults does trigger a recomp (adding a new type)
	cp A2.hs A.hs
	# Compile Main again, should force recompilation because of the exported named default
	'$(TEST_HC)' $(TEST_HC_OPTS) -c A.hs -Wno-duplicate-exports
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompExportedDefault.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) RecompExportedDefault.o A.o -o RecompExportedDefault
	./RecompExportedDefault

	# Test 2: Changing order of top-level declarations does not trigger recomp
	cp A3.hs A.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c A.hs -Wno-duplicate-exports
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompExportedDefault.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) RecompExportedDefault.o A.o -o RecompExportedDefault
	./RecompExportedDefault

	# Test 3: Changing the default does trigger recompilation (changing order of types)
	cp A4.hs A.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c A.hs -Wno-duplicate-exports
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompExportedDefault.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) RecompExportedDefault.o A.o -o RecompExportedDefault
	./RecompExportedDefault
