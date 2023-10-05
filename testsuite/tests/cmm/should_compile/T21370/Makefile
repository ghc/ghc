TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

## check if -ddump-cmm-verbose -ddump-to-file generates files
# for all cmm stages and dumps correspond to correct procs
T21370:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c test.cmm -finfo-table-map
	'$(TEST_HC)' $(TEST_HC_OPTS) -c test2.cmm -finfo-table-map
	'$(TEST_HC)' $(TEST_HC_OPTS) -c subdir/test.cmm -finfo-table-map
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Main.hs -finfo-table-map
	'$(TEST_HC)' $(TEST_HC_OPTS) Main.o test.o test2.o subdir/test.o


