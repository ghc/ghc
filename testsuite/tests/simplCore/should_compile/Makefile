TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T3990:
	$(RM) -f T3990.o T3990.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -O -c -ddump-simpl T3990.hs | grep 'test_case'
        # Grep output should show an unpacked constructor

T8832:
	$(RM) -f T8832.o T8832.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) $(T8832_WORDSIZE_OPTS) -O -c -ddump-simpl T8832.hs | grep '^[a-zA-Z0-9]\+ ='

T11155:
	$(RM) -f T11155.o T11155.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T11155.hs
	-nm T11155.o  | grep 'stg_ap_0_upd'
	# Expecting no output from the grep

T8274:
	$(RM) -f T8274.o T8274.hi
	# Set -dppr-cols to ensure things don't wrap
	'$(TEST_HC)' $(TEST_HC_OPTS) -O -c -ddump-simpl -dsuppress-uniques -dppr-cols=200 T8274.hs | grep '#'

T7865:
	$(RM) -f T7865.o T7865.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -dsuppress-uniques -O2 -c -ddump-simpl T7865.hs | grep expensive

T3055:
	$(RM) -f T3055.o T3055.hi T3055.simpl
	'$(TEST_HC)' $(TEST_HC_OPTS) -O -c T3055.hs -ddump-simpl > T3055.simpl
	grep -F 'I# [-28#]' T3055.simpl | sed 's/.*\(I# \[-28#\]\).*/\1/'

T5658b:
	$(RM) -f T5658b.o T5658b.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -O -c T5658b.hs -ddump-simpl | grep -c indexIntArray
# Trac 5658 meant that there were three calls to indexIntArray instead of two

T5776:
	$(RM) -f T5776.o T5776.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -O -c T5776.hs -ddump-rules | grep -c dEq

T3772:
	$(RM) -f T3772*.hi T3772*.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O T3772_A.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O T3772.hs -dsuppress-uniques -ddump-simpl

T4306:
	$(RM) -f T4306.hi T4306.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O T4306.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --show-iface T4306.hi | grep 'wupd ::'

T4201:
	$(RM) -f T4201.hi T4201.o T4201.list
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O T4201.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --show-iface T4201.hi > T4201.list
	# poor man idea about how to replace GNU grep -B2 "Sym" invocation with pure POSIX tools
	for i in `grep -n "Sym" T4201.list |cut -d ':' -f -1`; do head -$$i T4201.list | tail -3 ; done
	$(RM) -f T4201.list

# This one looped as a result of bogus specialisation
T4903:
	$(RM) -f T4903.hi T4903a.hi T4903a.o T4903.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O T4903a.hs -dcore-lint
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O T4903.hs -dcore-lint

T4918:
	$(RM) -f T4918.hi T4918.o T4918a.hi T4918a.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O T4918a.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O T4918.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --show-iface T4918.hi | grep 'C#'

EvalTest:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O EvalTest.hs -ddump-simpl -dsuppress-uniques | grep 'rght.*Dmd' | sed 's/^ *//'

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

.PHONY: simpl021
simpl021:
	$(RM) -f Simpl021A.hi Simpl021A.o Simpl021B.hi Simpl021B.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Simpl021A.hs -O
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Simpl021B.hs -O

.PHONY: T5327
T5327:
	$(RM) -f T5327.hi T5327.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T5327.hs -O -ddump-simpl | grep -c '># 34# '

.PHONY: T5623
T5623:
	$(RM) -f T5623.hi T5623.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T5623.hs -O -ddump-prep | grep -c "plusAddr#"

.PHONY: T4138
T4138:
	$(RM) -f T4138.hi   T4138.o
	$(RM) -f T4138_A.hi T4138_A.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -O -c T4138_A.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -O -c T4138.hs -ddump-simpl > T4138.simpl
	grep -c 'F#' T4138.simpl

T7165:
	$(RM) -f T7165.hi T7165a.hi T7165a.o T7165.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O2 T7165a.hs -dcore-lint
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O2 T7165.hs -dcore-lint

T5366:
	$(RM) -f T5366.hi T5366.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -ddump-simpl -dsuppress-uniques -c -O T5366.hs | grep 'Bar.*I#'

# We expect 0 occurrences of ' $sgo' in -ddump-prep. Mind the Makefile
# $-escaping. The -grep ignores grep's expected exit code of 1.
.PHONY: T7796
T7796:
	'$(TEST_HC)' $(TEST_HC_OPTS) -O1 -c T7796.hs -ddump-prep > T7796.prep
	(grep -q '$$w$$sgo' T7796.prep || echo 'could not find $$w$$sgo; compilation was not as expected')
	-grep -c ' $$sgo' T7796.prep

T7995:
	$(RM) -f T7995.hi T7995.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -package ghc -ddump-rule-firings -c -O T7995.hs | grep 'text/str'

T8221:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O -fforce-recomp T8221a.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O -fforce-recomp T8221.hs

T5996:
	$(RM) -f T5996.o T5996.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -O -c T5996.hs -ddump-simpl -dsuppress-uniques -dsuppress-all | grep y2

T10083:
	$(RM) -f T10083.o T10083.hi T10083.hi-boot T10083a.o T10083a.hi 
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O T10083.hs-boot
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O T10083a.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O T10083.hs

.PHONY: T5615
T5615:
	$(RM) -f T5615.o T5615.hi T5615.dump-simpl
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O T5615.hs -ddump-simpl -ddump-to-file
	-grep 'divInt#' T5615.dump-simpl
	-grep 'modInt#' T5615.dump-simpl
	-grep 'quotInt#' T5615.dump-simpl
	-grep 'remInt#' T5615.dump-simpl
	grep -c '1999#' T5615.dump-simpl
