TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T15894:
	'$(TEST_CC)' -O2 -fno-builtin -c copysign.c -o copysign.o
	'$(AR)' rsc libcopysign.a copysign.o
	echo main | LIBRARY_PATH="$(PWD)" '$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) main.hs -lcopysign
