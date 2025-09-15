TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Recompilation tests

clean:
	rm -f *.o *.hi
	rm -f B.hs C

# 001: removing an export should force a retypecheck of dependent modules.

retc001: clean
	cp B1.hs B.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -fno-code -fwrite-interface --make C.hs
	echo 'Middle'
	'$(TEST_HC)' $(TEST_HC_OPTS) -fno-code -fwrite-interface --make C.hs
	echo 'End'
	cp B2.hs B.hs
	-'$(TEST_HC)' $(TEST_HC_OPTS) -fno-code -fwrite-interface --make C.hs
