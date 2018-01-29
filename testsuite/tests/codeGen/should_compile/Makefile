TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T2578:
	'$(TEST_HC)' $(TEST_HC_OPTS) --make T2578 -fforce-recomp -v0

debug:
	# Without optimisations, we should get annotations for basically
	# all expressions in the example program.
	echo == Dbg ==
	'$(TEST_HC)' $(TEST_HC_OPTS) debug -fforce-recomp -g -ddump-cmm-verbose \
		| grep -o src\<debug.hs:.*\> | sort -u
	./debug

	# With optimisations we will get fewer annotations.
	echo == Dbg -O2 ==
	'$(TEST_HC)' $(TEST_HC_OPTS) debug -fforce-recomp -g -ddump-cmm-verbose -O2 \
		> debug.cmm
	cat debug.cmm | grep -o src\<debug.hs:.*\> | sort -u

	# Common block elimination should merge the blocks
	# corresponding to alternatives 1 and 2, therefore there
	# must be a block containing exactly these two annotations
	# directly next to each other.
	echo == CBE ==
	cat debug.cmm | grep -A1 -B1 src\<debug.hs:3:.*\> \
                  | grep src\<debug.hs:4:.*\> \
                  | grep -o src\<.*\> | sort -u

	./debug
	rm debug

T14999:
	'$(TEST_HC)' $(TEST_HC_OPTS) -O2 -g -c T14999.cmm -o T14999.o
	gdb --batch -ex 'file T14999.o' -ex 'disassemble stg_catch_frame_info' --nx | tr -s '[[:blank:]\n]'
	readelf --debug-dump=frames-interp T14999.o | tr -s '[[:blank:]\n]'

T15196:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O -ddump-asm T15196.hs | grep "jp " ; echo $$?

T15723:
	'$(TEST_HC)' $(TEST_HC_OPTS) -prof -fPIC -fexternal-dynamic-refs -fforce-recomp -O2 -c T15723A.hs -o T15723A.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -prof -fPIC -fexternal-dynamic-refs -fforce-recomp -O2 -c T15723B.hs -o T15723B.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -dynamic -shared T15723B.o -o T15723B.so

## check that there are two assembly equates
# mentioning T15155.a_closure (def and use)
T15155:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O0 -ddump-asm T15155l.hs | grep -F ".equiv " \
	| grep -F "T15155.a_closure" | wc -l | sed -e 's/ *//g' | grep "2" ; echo $$?

## check that there are two "$def" aliases:
#  - one that bitcasts to %T15155_a_closure_struct*
#  - and the other which bitcasts from %T15155_a_closure_struct*
##
T15155l:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -fllvm -O0 -ddump-llvm T15155l.hs 2>/dev/null \
	| grep -F "= alias %T15155_" | grep -E "@T15155_[ab]_closure.def = " | grep -F "%T15155_a_closure_struct*" \
	| wc -l | sed -e 's/ *//g' | grep "2"; echo $$?
