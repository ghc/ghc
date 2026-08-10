TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:

recomp023: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -bytecodelib -fbyte-code \
		-fwrite-interface -fwrite-byte-code -this-unit-id=recomp023 \
		-o recomp023.bytecodelib M.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -bytecodelib -fbyte-code \
		-fwrite-interface -fwrite-byte-code -this-unit-id=recomp023 \
		-o recomp023.bytecodelib M.hs
