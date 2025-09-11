TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T18733:
	cp Library1.hs Library.hs
	'$(TEST_HC)' $(EXTRA_HC_OPTS) -v0 -o Main Library.hs Main.hs
	./Main
	
	cp Library2.hs Library.hs
	'$(TEST_HC)' $(EXTRA_HC_OPTS) -v0 -o Main Library.hs Main.hs
	./Main
