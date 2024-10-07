TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T24495:
	'$(TEST_HC)' $(TEST_HC_OPTS) T24495.hs -v0 -O1 -dsuppress-uniques -ddump-js -ddump-to-file
	./T24495
	# check that the optimization occurred for -01 3 times (2 for cases + 1 for unfloated lits)
	grep -c appendToHsStringA T24495.dump-js

	'$(TEST_HC)' $(TEST_HC_OPTS) T24495.hs -v0 -O2 -dsuppress-uniques -ddump-js -ddump-to-file
	./T24495
	# check that the optimization occurred for -02 1 time (1 for unfloated lits)
	grep -c appendToHsStringA T24495.dump-js

T23479_1:
	'$(TEST_HC)' $(TEST_HC_OPTS) T23479_1.hs -v0 -O2 -dsuppress-uniques -ddump-js -ddump-to-file
	./T23479_1
	# check that the optimization occurred
	grep -c "h\$$r1 = \"test_val_1\"" T23479_1.dump-js
	grep -c "h\$$r1 = \"test_val_2\"" T23479_1.dump-js
	grep -c "h\$$r1 = \"test_val_3\"" T23479_1.dump-js
	grep -c "h\$$r1 = \"test_val_80_local" T23479_1.dump-js
	grep -c "h\$$r1 = \"test_val_80_global" T23479_1.dump-js || true

T23479_2:
	'$(TEST_HC)' $(TEST_HC_OPTS) T23479_2.hs -v0 -O2 -dsuppress-uniques -ddump-js -ddump-to-file
	./T23479_2
	grep -c "h\$$r1 = \"test_val_1\"" T23479_2.dump-js
	grep -c "h\$$r1 = \"test_val_80_local_once" T23479_2.dump-js
	# check that the optimization occurred
	grep -c "h\$$r1 = h\$$decodeUtf8z" T23479_2.dump-js
