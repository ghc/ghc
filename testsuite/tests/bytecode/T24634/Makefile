TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T24634:
	$(TEST_HC) -c -dynamic hello_c.c -o hello_c.o
	$(TEST_HC) -c -fbyte-code-and-object-code Hello.hs
	$(TEST_HC) -fbyte-code-and-object-code -fprefer-byte-code hello_c.o Main.hs
	./Main
