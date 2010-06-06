TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -f *.o *.hi

apirecomp001: clean
	'$(TEST_HC)' --make -v0 -package ghc myghc.hs
	./myghc "`'$(TEST_HC)' --print-libdir | tr -d '\r'`"

