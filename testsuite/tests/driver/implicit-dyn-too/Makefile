TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -f *.o *.hi *.dyn_hi *.dyn_o

implicit-dyn-too: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) "QuasiQuote.hs" -fno-code -fwrite-interface -v1 --make
	rm "QuasiExpr.dyn_o"
	'$(TEST_HC)' $(TEST_HC_OPTS) "QuasiQuote.hs" -fno-code -fwrite-interface -v1 --make

