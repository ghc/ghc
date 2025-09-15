TOP=../..

LIBDIR := "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

codes:
	(cd $(TOP)/.. && $(LINT_CODES) test $(LIBDIR))
