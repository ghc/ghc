TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -f *.o
	rm -f *.hi
	rm -f Main

main:
	rm -f *.o
	rm -f *.hi
	rm -f Main
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Lib.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) Main.hs
	./Main
