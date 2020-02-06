TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

## check if -ddump-cmm-verbose -ddump-to-file generates files
# for all cmm stages and dumps correspond to correct procs
T16930:
	echo "testing -ddump-cmm-verbose for T16930 ..."
	'$(TEST_HC)' $(TEST_HC_OPTS) T16930.hs -fforce-recomp -ddump-cmm-verbose -ddump-to-file
	grep -rl "CAFEnv" . --include=\T16930.*
	grep -rl "Post control-flow optimisations" . --include=\T16930.*
	grep -rl "Post CPS Cmm" . --include=\T16930.*
	grep -rl "after setInfoTableStackMap" . --include=\T16930.*
	grep -rl "Layout Stack" . --include=\T16930.*
	grep -rl "Post switch plan" . --include=\T16930.*
