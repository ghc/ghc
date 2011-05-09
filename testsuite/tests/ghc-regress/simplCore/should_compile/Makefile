TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T3055:
	$(RM) -f T3055.o T3055.hi T3055.simpl
	'$(TEST_HC)' $(TEST_HC_OPTS) -O -c T3055.hs -ddump-simpl > T3055.simpl
	grep 'I# (-28)' T3055.simpl | sed 's/.*\(I# (-28)\).*/\1/'

T3772:
	$(RM) -f T3772*.hi T3772*.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O T3772_A.hs 
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O T3772.hs -dsuppress-uniques -ddump-simpl

T4306:
	$(RM) -f T4306.hi T4306.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O T4306.hs 
	'$(TEST_HC)' $(TEST_HC_OPTS) --show-iface T4306.hi | grep 'wupd ::'

T4201:
	$(RM) -f T4201.hi T4201.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O T4201.hs 
	'$(TEST_HC)' $(TEST_HC_OPTS) --show-iface T4201.hi | grep 'Sym'

# This one looped as a result of bogus specialisation
T4903:
	$(RM) -f T4903a.o T4903.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O T4903a.hs -dcore-lint
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O T4903.hs -dcore-lint

T4918:
	$(RM) -f T4918.hi T4918.o T4918a.hi T4918a.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O T4918a.hs 
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O T4918.hs 
	'$(TEST_HC)' $(TEST_HC_OPTS) --show-iface T4918.hi | grep 'C#'

EvalTest:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O EvalTest.hs -ddump-simpl -dsuppress-uniques | grep 'rght.*Dmd'

T5168:
	-('$(TEST_HC)' $(TEST_HC_OPTS) -c -O T5168.hs -ddump-simpl -dsuppress-uniques | grep 'patError')

# When SpecConstr works there are no STUArrays at all
# The "-(...)" ignores the (expected) non-zero exit code from grep
# when there are (as expected) no matches
T4945:
	-('$(TEST_HC)' $(TEST_HC_OPTS) -c -O2 -fno-liberate-case T4945.hs -ddump-simpl -dsuppress-uniques | grep 'STUArray')

# When this one works there are no 'lets' at all
T4957:
	-('$(TEST_HC)' $(TEST_HC_OPTS) -c -O T4957.hs -ddump-simpl -dsuppress-uniques | grep 'let')
