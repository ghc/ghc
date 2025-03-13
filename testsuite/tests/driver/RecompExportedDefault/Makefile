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
	# Now modify the A module to include an exported named default
	cp A2.hs A.hs
	# Compile Main again, should force recompilation because of the exported named default
	'$(TEST_HC)' $(TEST_HC_OPTS) -c RecompExportedDefault.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) RecompExportedDefault.o A.o -o RecompExportedDefault
	./RecompExportedDefault
