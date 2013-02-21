# This Makefile runs the tests using GHC's testsuite framework.  It
# assumes the package is part of a GHC build tree with the testsuite
# installed in ../../../testsuite.

TOP=../../../../testsuite
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

test.concio001:
	$(TEST_HC) $(TEST_HC_OPTS) --make -fforce-recomp -v0 concio001 -o concio001 
	(sleep 1; echo x) | ./concio001

test.concio001.thr:
	$(TEST_HC) $(TEST_HC_OPTS) --make -fforce-recomp -v0 -threaded concio001 -o concio001 
	(sleep 1; echo x) | ./concio001

# NB. utf8-test should *not* have a final newline.  The last char should be 'X'.
utf16-test: utf8-test
	iconv -f UTF-8 -t UTF-16 <utf8-test >utf16-test

utf16le-test: utf8-test
	iconv -f UTF-8 -t UTF-16LE <utf8-test >utf16le-test

utf16be-test: utf8-test
	iconv -f UTF-8 -t UTF-16BE <utf8-test >utf16be-test

utf32-test: utf8-test
	iconv -f UTF-8 -t UTF-32 <utf8-test >utf32-test

utf32le-test: utf8-test
	iconv -f UTF-8 -t UTF-32LE <utf8-test >utf32le-test

utf32be-test: utf8-test
	iconv -f UTF-8 -t UTF-32BE <utf8-test >utf32be-test

utf8-bom-test: utf16-test
	iconv -f UTF-16LE -t UTF-8 <utf16-test >utf8-bom-test

hSetEncoding001.in : latin1 utf8-test utf16le-test utf16be-test utf16-test utf32le-test utf32be-test utf32-test utf8-bom-test
	cat >$@ latin1 utf8-test utf16le-test utf16be-test utf16-test utf32-test utf32le-test utf32be-test utf8-bom-test

environment001-test:
	"$(TEST_HC)" --make -fforce-recomp -v0 environment001.hs -o environment001 
	GHC_TEST=马克斯 ./environment001 说

T3307-test:
	"$(TEST_HC)" --make -fforce-recomp -v0 T3307.hs -o T3307
	echo Ni hao > chinese-file-小说
	echo chinese-file-小说 > chinese-name
	# The tests are run in whatever the default locale is. This is almost always UTF-8,
	# but in cmd on Windows it will be the non-Unicode CP850 locale.
	./T3307 chinese-file-小说
