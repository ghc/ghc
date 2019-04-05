TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -f *.o
	rm -f *.hi
	rm -f Main

# check that the compilation progress message will contain
# *.dyn_o file with -dynamic-too
main:
	rm -f *.o
	rm -f *.hi
	rm -f Main
	'$(TEST_HC)' $(TEST_HC_OPTS) -dynamic-too Main.hs
