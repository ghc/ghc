TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T20010:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T20010.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T20010_c.cpp
	'$(TEST_HC)' $(TEST_HC_OPTS) -package system-cxx-std-lib T20010_c.o T20010.o -o T20010
	./T20010

T20010-ghci:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T20010.hs
	# We must build with PIC lest we end up with R_X86_64_32 relocations in
	# .rodata which precludes linking into a shared object and therefore loading
	# in GHCi
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -fPIC T20010_c.cpp
	'$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) -package system-cxx-std-lib T20010_c.o <T20010-ghci.script

