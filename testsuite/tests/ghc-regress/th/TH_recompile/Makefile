TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Test for trac #481. GHC should notice that the code that generates
# the splice has changed, and thus re-run it.

clean:
	rm -f *.o
	rm -f hi
	rm -f test test2
	rm -f Sub.hs

TH_recompile:
	$(MAKE) clean
	cp Sub1.hs Sub.hs
	ghc -v0 --make -fth Main.hs -o test
	cp Sub2.hs Sub.hs
	ghc -v0 --make -fth Main.hs -o test2
	./test
	./test2

