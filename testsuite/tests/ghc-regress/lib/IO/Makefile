TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

test.concio001:
	@$(TEST_HC) $(TEST_HC_OPTS) --make -fforce-recomp -v0 concio001 -o concio001 
	@(sleep 1; echo x) | ./concio001

test.concio001.thr:
	@$(TEST_HC) $(TEST_HC_OPTS) --make -fforce-recomp -v0 -threaded concio001 -o concio001 
	@(sleep 1; echo x) | ./concio001

# NB. utf8-test should *not* have a final newline.  The last char should be 'X'.
utf16-test: utf8-test
	iconv -f UTF8 -t UTF16 <utf8-test >utf16-test

utf16le-test: utf8-test
	iconv -f UTF8 -t UTF16LE <utf8-test >utf16le-test

utf16be-test: utf8-test
	iconv -f UTF8 -t UTF16BE <utf8-test >utf16be-test

utf32-test: utf8-test
	iconv -f UTF8 -t UTF32 <utf8-test >utf32-test

utf32le-test: utf8-test
	iconv -f UTF8 -t UTF32LE <utf8-test >utf32le-test

utf32be-test: utf8-test
	iconv -f UTF8 -t UTF32BE <utf8-test >utf32be-test

utf8-bom-test: utf16-test
	iconv -f UTF16LE -t UTF8 <utf16-test >utf8-bom-test

hSetEncoding001.in : latin1 utf8-test utf16le-test utf16be-test utf16-test utf32le-test utf32be-test utf32-test utf8-bom-test
	cat >$@ latin1 utf8-test utf16le-test utf16be-test utf16-test utf32-test utf32le-test utf32be-test utf8-bom-test
