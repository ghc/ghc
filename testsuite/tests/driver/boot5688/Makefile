TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -f *.o *.hi *.hi-boot *.o-boot

boot5688: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) --make  A.hs -fno-code -fwrite-interface
	'$(TEST_HC)' $(TEST_HC_OPTS) -c  A.hs
