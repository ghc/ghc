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

T23479:
	'$(TEST_HC)' $(TEST_HC_OPTS) T23479.hs -v0 -O2 -dsuppress-uniques -ddump-js -ddump-to-file
	./T23479
	# check that the optimization occurred
	grep -c "h\$$r1 = \"test_val_1\"" T23479.dump-js
	grep -c "h\$$r1 = \"test_val_2\"" T23479.dump-js
	grep -c "h\$$r1 = \"test_val_3\"" T23479.dump-js
	grep -c "h\$$r1 = \"test_val_80_local" T23479.dump-js
	grep -c "h\$$r1 = \"test_val_80_global" T23479.dump-js || true

T24744:
	'$(TEST_HC)' $(TEST_HC_OPTS) T24744.hs -v0 -O2 -dsuppress-uniques -ddump-js -ddump-to-file
	./T24744
	grep -c "h\$$r1 = \"test_val_1\"" T24744.dump-js
	grep -c "h\$$r1 = \"test_val_80_local_once" T24744.dump-js
	# check that the optimization occurred
	grep -c "h\$$r1 = h\$$decodeUtf8z" T24744.dump-js
	grep -c "h\$$stc(h\$$mainZCMainzitestzuvalzu80zulocal,h\$$mainZCMainzitestzuvalzu80zulocal_e,\[h\$$ghczmprimZCGHCziCStringziunpackCStringzh,h\$$mainZCMainzimain7_1,h\$$mainZCMainzimain7_2\])" T24744.jsexe/out.js
