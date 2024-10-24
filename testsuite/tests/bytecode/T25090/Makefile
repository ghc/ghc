TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Verify that the object files aren't linked by clobbering them.
T25090a:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -fbyte-code-and-object-code C.hs-boot
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -fbyte-code-and-object-code B.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -fbyte-code-and-object-code C.hs
	echo 'corrupt' > B.o
	echo 'corrupt' > C.o
	echo 'corrupt' > C.o-boot
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -fbyte-code-and-object-code D.hs
	echo 'corrupt' > D.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -fbyte-code-and-object-code -fprefer-byte-code A.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -fbyte-code-and-object-code -fprefer-byte-code A.o -o exe
	./exe

T25090b:
	'$(TEST_HC)' $(TEST_HC_OPTS) -fbyte-code-and-object-code -fprefer-byte-code A -o exe -v0
	./exe
