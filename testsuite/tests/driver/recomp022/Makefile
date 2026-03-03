TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Recompilation test for .hs which generate different bytecode objects

clean:

recomp022a: clean
	mkdir src
	mv B.hs  src/B.hs
	mv A1.hs src/A.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -fbyte-code-and-object-code -no-link -fwrite-byte-code -fprefer-byte-code -i -isrc B A
	mv A2.hs src/A.hs

	# While A.hi changes, A.gbc stays the same, thus we don't have to recompile B,
	# as B only uses byte code from A.
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -fbyte-code-and-object-code -no-link -fwrite-byte-code -fprefer-byte-code -i -isrc B A

recomp022b: clean
	mkdir src
	mv B.hs  src/B.hs
	mv C.hs  src/C.hs
	mv A1.hs src/A.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -no-link -fbyte-code-and-object-code -fwrite-byte-code -fprefer-byte-code -i -isrc B A C
	mv A3.hs src/A.hs
	# C doesn't need to be recompiled, as the ABI of A doesn't change
	# B still needs to be recompiled, since the bytecode objects change
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -no-link -fbyte-code-and-object-code -fwrite-byte-code -fprefer-byte-code -i -isrc B A C
