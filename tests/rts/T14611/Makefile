TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T14611:
	'$(TEST_CC)' -c foo.c -o foo.o
	'$(AR)' rsc libfoo.a foo.o
	'$(TEST_HC)' -shared foo_dll.c -o libfoo-1.dll
	mv libfoo-1.dll.a libfoo.dll.a
	echo main | LIBRARY_PATH="$(PWD)" '$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) main.hs -lfoo
