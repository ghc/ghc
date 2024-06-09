TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

cpp_assertions_ignored:
	echo "Without -fignore-asserts"
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 main.hs
	(./main 2>&1); true
	echo "With -fignore-asserts"
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -fignore-asserts main.hs
	./main 2>&1
