TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# This case loads bytecode from the interface file written in the second invocation.
T24634a:
	'$(TEST_HC)' -c hello_c.c -o hello_c.o
	'$(TEST_HC)' -c -fbyte-code-and-object-code -fno-omit-interface-pragmas Hello.hs
	'$(TEST_HC)' -fprefer-byte-code -fbyte-code-and-object-code -fno-ignore-interface-pragmas hello_c.o Main.hs
	./Main

# This case uses the bytecode generated in 'runHscBackendPhase', not involving the interface, since 'Hello' is compiled
# in the same invocation as 'Main'.
T24634b:
	'$(TEST_HC)' -c hello_c.c -o hello_c.o
	'$(TEST_HC)' -fprefer-byte-code -fbyte-code-and-object-code -fno-ignore-interface-pragmas hello_c.o Hello.hs Main.hs
	./Main
