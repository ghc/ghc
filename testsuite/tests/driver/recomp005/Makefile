TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -f *.o*
	rm -f *.hi*
	rm -f C.hs

recomp005: clean
	cp C1.hs C.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --make E.hs -O
	sleep 1
	cp C2.hs C.hs
	# Should recompile E, because C is now an orphan
	'$(TEST_HC)' $(TEST_HC_OPTS) --make E.hs -O
