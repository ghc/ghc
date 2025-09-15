TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Recompilation test for when .hi files are up to date but .hs files have been moved


recomp020:
	mkdir src1
	mkdir src2
	cp A.hs src1/A.hs
	cp A.hs src2/A.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -S src1/A.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -S src2/A.hs
	diff src1/A.hi src2/A.hi
