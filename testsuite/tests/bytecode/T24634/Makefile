TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

bytecode-capi:
	$(TEST_HC) -c hello.c
	$(TEST_HC) -c -fbyte-code-and-object-code Hello.hs
	$(TEST_HC) -fprefer-byte-code hello.o Main.hs
	./Main
