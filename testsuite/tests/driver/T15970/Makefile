TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -f *.o *.hi
	rm -f A.hs C

# Changing something that a default method depends on should force
# recompilation of a module that instantiates the class.

T15970: clean
	cp A1.hs A.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -O2 C.hs
	sleep 1
	cp A2.hs A.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -O2 C.hs
