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
