TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T24495:
	'$(TEST_HC)' $(TEST_HC_OPTS) T24495.hs -v0 -O1 -dsuppress-uniques -ddump-js -ddump-to-file
	./T24495
	# check that the optimization occurred
	grep -c appendToHsStringA T24495.dump-js

T23479:
	'$(TEST_HC)' $(TEST_HC_OPTS) T23479.hs -v0 -O1 -dsuppress-uniques -ddump-js -ddump-to-file
	./T23479
	# check that the optimization occurred
	grep -c " h\$$decodeUtf8z" T23479.dump-js
