TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Recompilation test for .hs files with CPP includes

clean:

recomp021: clean
	mkdir src1
	mkdir src2
	cp *.hs src1
	cp *.hs src2
	'$(TEST_HC)' $(TEST_HC_OPTS) -S src1/A.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -S src2/A.hs
	# Suppress stderr since messages may differ between diff versions
	! diff src1/A.hi src2/A.hi >/dev/null 2>&1
