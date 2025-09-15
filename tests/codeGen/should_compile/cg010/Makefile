TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Make sure LFInfo causes the imported reference to val to get tagged.
cg010:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O A.hs -fforce-recomp
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O Main.hs -fforce-recomp -ddump-cmm -ddump-to-file
	grep "A.val_closure+2" Main.dump-cmm
