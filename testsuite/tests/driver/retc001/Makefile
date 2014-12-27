TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# -fforce-recomp makes lots of driver tests trivially pass, so we
# filter it out from $(TEST_HC_OPTS).
TEST_HC_OPTS_NO_RECOMP = $(filter-out -fforce-recomp,$(TEST_HC_OPTS))

# Recompilation tests

clean:
	rm -f *.o *.hi
	rm -f B.hs C

# 001: removing an export should force a retypecheck of dependent modules.

retc001: clean
	cp B1.hs B.hs
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) -fno-code -fwrite-interface --make C.hs
	echo 'Middle'
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) -fno-code -fwrite-interface --make C.hs
	echo 'End'
	cp B2.hs B.hs
	-'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) -fno-code -fwrite-interface --make C.hs
