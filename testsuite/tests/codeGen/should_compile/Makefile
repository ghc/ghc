TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T2578:
	'$(TEST_HC)' $(TEST_HC_OPTS) --make T2578 -fforce-recomp -v0

debug:
	# Without optimisations, we should get annotations for basically
	# all expressions in the example program.
	echo == Dbg ==
	'$(TEST_HC)' $(TEST_HC_OPTS) debug -fforce-recomp -g -ddump-cmm-verbose-by-proc \
		| grep -o src\<debug.hs:.*\> | sort -u
	./debug

	# With optimisations we will get fewer annotations.
	echo == Dbg -O2 ==
	'$(TEST_HC)' $(TEST_HC_OPTS) debug -fforce-recomp -g -ddump-cmm-verbose-by-proc -O2 \
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
	LANG=C readelf --debug-dump=frames-interp T14999.o | tr -s '[[:blank:]\n]'

T15196:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O -ddump-asm T15196.hs | grep "jp " ; echo $$?

T15723:
	'$(TEST_HC)' $(TEST_HC_OPTS) -prof -fPIC -fexternal-dynamic-refs -fforce-recomp -O2 -c T15723A.hs -o T15723A.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -prof -fPIC -fexternal-dynamic-refs -fforce-recomp -O2 -c T15723B.hs -o T15723B.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -dynamic -shared T15723B.o -o T15723B.so

# Check that the static indirection b is compiled to an equiv directive
T15155:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O0 -ddump-asm T15155l.hs | \
		grep -F ".equiv T15155.b_closure,T15155.a_closure"

# Same as above, but in LLVM. Check that the static indirection b is compiled to
# an alias.
T15155l:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O0 -ddump-llvm T15155l.hs 2>/dev/null | \
		grep -F "@T15155_b_closure = alias i8, i8* @T15155_a_closure"

# Without -fcatch-bottoms `f` is non-CAFFY. With -fcatch-bottoms it becomes
# CAFFY. Before CafInfo rework (c846618a) this used to cause incorrect CafInfo
# attached to `f` in the interface file.
T17648:
	# -dno-typeable-binds is to make the iface simpler
	#  -O is necessary as otherwise we don't write interface pragmas (e.g.
	#  NoCafRefs) to the interface files.
	'$(TEST_HC)' $(TEST_HC_OPTS) -dno-typeable-binds -O T17648.hs -v0
	'$(TEST_HC)' --show-iface T17648.hi | tr -d '\n\r' | \
		grep -F 'f :: T GHC.Types.Int -> ()  [HasNoCafRefs, LambdaFormInfo' >/dev/null

	# Second compilation with -fcatch-bottoms, f should be CAFFY
	'$(TEST_HC)' $(TEST_HC_OPTS) -dno-typeable-binds -O \
		-fcatch-bottoms T17648.hs -v0 -fforce-recomp
	'$(TEST_HC)' --show-iface T17648.hi | tr -d '\n\r' | \
		grep -F 'f :: T GHC.Types.Int -> ()  [LambdaFormInfo' >/dev/null
