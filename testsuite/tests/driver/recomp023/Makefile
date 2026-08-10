TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Recompilation test for bytecode libraries: the library is only relinked
# when one of its constituents changes

BUILD = '$(TEST_HC)' $(TEST_HC_OPTS) --make -bytecodelib -fbyte-code \
	-fwrite-interface -fwrite-byte-code -this-unit-id=recomp023 \
	-o recomp023.bytecodelib M.hs

clean:

recomp023: clean
	echo "first build"
	$(BUILD)
	echo "second build"
	$(BUILD)
	mv M2.hs M.hs
	echo "third build"
	$(BUILD)
