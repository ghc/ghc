TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T24634:
	$(TEST_HC) -c -dynamic hello.c -o hello.o
	$(TEST_HC) -c -fbyte-code-and-object-code Hello.hs
	$(TEST_HC) -fprefer-byte-code hello.o Main.hs
	./Main
