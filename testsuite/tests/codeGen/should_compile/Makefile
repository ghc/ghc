TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T2578:
	'$(TEST_HC)' $(TEST_HC_OPTS) --make T2578 -fforce-recomp -v0

T14626:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O -ddump-prep -dsuppress-uniques T14626.hs | grep case

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
