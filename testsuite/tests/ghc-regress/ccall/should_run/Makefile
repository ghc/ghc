TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

ffi018_ghci_setup :
	cp ffi018_c.c ffi018_ghci_c.c
	$(TEST_HC) -c ffi018_ghci_c.c
