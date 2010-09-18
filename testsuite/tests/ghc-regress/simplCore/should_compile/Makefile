TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T3055:
	$(RM) -f T3055.o T3055.hi T3055.simpl
	'$(TEST_HC)' -O -c T3055.hs -ddump-simpl > T3055.simpl
	grep 'I# (-28)' T3055.simpl | sed 's/.*\(I# (-28)\).*/\1/'

T3772:
	$(RM) -f T3772*.hi T3772*.o
	'$(TEST_HC)' -c -O T3772_A.hs 
	'$(TEST_HC)' -c -O T3772.hs -dsuppress-uniques -ddump-simpl

T4306:
	$(RM) -f T4306.hi T4306.o
	'$(TEST_HC)' -c -O T4306.hs 
	'$(TEST_HC)' --show-iface T4306.hi | grep 'wupd ::'

T4201:
	$(RM) -f T4201.hi T4201.o
	'$(TEST_HC)' -c -O T4201.hs 
	'$(TEST_HC)' --show-iface T4201.hi | grep 'Unfolding.*sym'

