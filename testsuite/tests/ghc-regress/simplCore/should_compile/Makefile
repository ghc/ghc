TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T3055:
	$(RM) -f T3055.o T3055.hi T3055.simpl
	'$(TEST_HC)' -O -c T3055.hs -ddump-simpl > T3055.simpl
	grep 'I# (-28)' T3055.simpl | sed 's/.*\(I# (-28)\).*/\1/'

