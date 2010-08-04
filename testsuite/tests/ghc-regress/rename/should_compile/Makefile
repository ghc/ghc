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

T4003:
	$(RM) T4003A.hi-boot T4003A.hi T4003B.hi
	$(RM) T4003A.o-boot  T4003A.o  T4003B.o
	'$(TEST_HC)' -O -c T4003A.hs-boot
	'$(TEST_HC)' -O -c T4003B.hs
	'$(TEST_HC)' -O -c T4003A.hs

T3449:
	$(RM) T3449.hi-boot T3449.hi T3449A.hi
	$(RM) T3449.o-boot  T3449.o  T3449A.o
	'$(TEST_HC)' -Wall -c T3449.hs-boot
	'$(TEST_HC)' -Wall -c T3449A.hs
	'$(TEST_HC)' -Wall -c T3449.hs

T4239:
	$(RM) T4239A.hi T4239A.o
	$(RM) T4239.hi  T4239.o T4239.imports
	'$(TEST_HC)' -c T4239A.hs
	'$(TEST_HC)' -c T4239.hs -ddump-minimal-imports
	cat T4239.imports

