TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T25090a:
	$(TEST_HC) -c -fbyte-code-and-object-code C.hs-boot
	$(TEST_HC) -c -fbyte-code-and-object-code B.hs
	$(TEST_HC) -c -fbyte-code-and-object-code C.hs
	$(TEST_HC) -c -fbyte-code-and-object-code D.hs
	$(TEST_HC) -c -fbyte-code-and-object-code -fprefer-byte-code A.hs
	$(TEST_HC) -fbyte-code-and-object-code -fprefer-byte-code D.o C.o B.o A.o -o exe
	./exe

T25090b:
	$(TEST_HC) -fbyte-code-and-object-code -fprefer-byte-code A -o exe -v0
	./exe
