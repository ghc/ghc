TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: T27532
T27532:
	'$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) -ignore-dot-ghci -v0 < a.script 2>/dev/null | grep '^instance' > direct.txt
	'$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) -ignore-dot-ghci -v0 < b.script 2>/dev/null | grep '^instance' > afterimport.txt
	diff direct.txt afterimport.txt || true

.PHONY: T27532j4
T27532j4:
	printf ':info ()\n' | '$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) -ignore-dot-ghci -v0 -j4 -w M*.hs 2>/dev/null | grep '^instance' > j4_1.txt
	printf ':info ()\n' | '$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) -ignore-dot-ghci -v0 -j4 -w M*.hs 2>/dev/null | grep '^instance' > j4_2.txt
	printf ':info ()\n' | '$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) -ignore-dot-ghci -v0 -j4 -w M*.hs 2>/dev/null | grep '^instance' > j4_3.txt
	printf ':info ()\n' | '$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) -ignore-dot-ghci -v0 -j4 -w M*.hs 2>/dev/null | grep '^instance' > j4_4.txt
	printf ':info ()\n' | '$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) -ignore-dot-ghci -v0 -j4 -w M*.hs 2>/dev/null | grep '^instance' > j4_5.txt
	printf ':info ()\n' | '$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) -ignore-dot-ghci -v0 -j4 -w M*.hs 2>/dev/null | grep '^instance' > j4_6.txt
	diff j4_1.txt j4_2.txt || true
	diff j4_1.txt j4_3.txt || true
	diff j4_1.txt j4_4.txt || true
	diff j4_1.txt j4_5.txt || true
	diff j4_1.txt j4_6.txt || true
