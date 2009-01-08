TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

PKG=bug1465

clean:
	rm -f v1/setup v1/Setup.o v1/Setup.hi
	rm -f v2/setup v2/Setup.o v2/Setup.hi
	rm -rf v1/dist v2/dist
	rm -f *.o *.hi
	rm -f clean.out prep.out
	-$(GHC_PKG) unregister $(PKG)

# We aren't interested in the output from the cabal & compilation
# stages, so redirect it to a file.
bug1465:
	$(MAKE) clean 1>clean.out 2>clean.out
	$(MAKE) prep  1>prep.out 2>prep.out
	$(TEST_HC) -c C.hs || exit 0
	$(MAKE) clean 1>>clean.out 2>>clean.out

prep:
	for i in v1 v2; do \
	   (cd $$i && \
		$(TEST_HC) --make -o setup Setup.hs && \
		./setup configure -v0 --with-compiler=$(TEST_HC) --with-hc-pkg=$(GHC_PKG) && \
		./setup build && \
		./setup register --inplace ); done

	$(TEST_HC) -c -package $(PKG)-1.0 B1.hs
	$(TEST_HC) -c -package $(PKG)-2.0 B2.hs
