TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

## check if -ddump-cmm-verbose -ddump-to-file generates files
# for all cmm stages and dumps correspond to correct procs
T16930:
	echo "testing -ddump-cmm-verbose for T16930 ..."
	'$(TEST_HC)' $(TEST_HC_OPTS) T16930.hs -fforce-recomp -ddump-cmm-verbose -ddump-to-file
	grep -rl "CAFEnv" `ls T16930.*`
	grep -rl "Post control-flow optimisations" `ls T16930.*`
	grep -rl "Post CPS Cmm" `ls T16930.*`
	grep -rl "after setInfoTableStackMap" `ls T16930.*`
	grep -rl "Layout Stack" `ls T16930.*`
	grep -rl "Post switch plan" `ls T16930.*`

T23610:
	'$(TEST_HC)' $(TEST_HC_OPTS) T23610.cmm -S

# The three seds below, in order:
#  1. Keep only the "Parsed Cmm" dump, since that is the one stage where the
#     unreachable block still exists.
#  2. Rewrite goto targets: their label uniques survive -dsuppress-uniques
#     (#21310).
#  3. Drop the "// CmmAssign"-style node annotations, which pprNode emits
#     only on DEBUG compilers.
T27368-ppr-debug:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -no-hs-main -ddump-cmm-verbose-by-proc -dppr-debug -dsuppress-uniques -dsuppress-ticks T27368-ppr-debug.cmm 2>&1 \
	| sed -n '/^==* Parsed Cmm/,/^ \}\]/p' \
	| sed 's/goto c[0-9A-Za-z]*/goto _lbl_/g' \
	| sed 's| *// Cmm[A-Za-z]*$$||'
