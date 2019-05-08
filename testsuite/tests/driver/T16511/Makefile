TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -f *.o *.hi
	rm -f B.hs D

# If the dependencies list of a directly-imported module change, we should
# recompile our modules.

T16511: clean
	cp B1.hs B.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 Main.hs
	sleep 1
	cp B2.hs B.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 Main.hs
