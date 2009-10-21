TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T1792_imports:
	$(RM) T1792_imports.hi T1792_imports.o T1792_imports.imports
	'$(TEST_HC)' -ddump-minimal-imports -c T1792_imports.hs
	cat T1792_imports.imports

