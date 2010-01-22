TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T1792_imports:
	$(RM) T1792_imports.hi T1792_imports.o T1792_imports.imports
	'$(TEST_HC)' -ddump-minimal-imports -c T1792_imports.hs
	cat T1792_imports.imports

T3823:
	$(RM) T3823A.hi-boot T3823A.hi T3823B.hi
	$(RM) T3823A.o-boot  T3823A.o  T3823B.o
	-'$(TEST_HC)' -v0 --make T3823A.hs

