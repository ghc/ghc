TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: T13082_good
T13082_good:
	'$(TEST_CC)' -c foo.c -o foo.o
	'$(AR)' rsc libfoo.a foo.o
	'$(TEST_HC)' -shared foo_dll.c -o libfoo-1.dll
	mv libfoo-1.dll.a libfoo.dll.a
	echo main | '$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) main.hs -llibfoo -L"$(PWD)"

.PHONY: T13082_fail
T13082_fail:
	'$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) main.hs -ldoesnotexist 2>&1 | grep "addDLL: doesnotexist or dependencies not loaded"

