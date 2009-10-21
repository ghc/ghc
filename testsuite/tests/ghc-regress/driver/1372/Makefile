TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -f p1/setup p1/Setup.o p1/Setup.hi
	rm -f p2/setup p2/Setup.o p2/Setup.hi
	rm -rf p1/dist p2/dist
	rm -f *.o *.hi
	rm -f clean.out prep.out
	rm -f p1/A.hs
	-"$(GHC_PKG)" unregister p1
	-"$(GHC_PKG)" unregister p2

# We aren't interested in the output from the cabal & compilation
# stages, so redirect it to a file.
1372:
	$(MAKE) clean 1>clean.out 2>clean.out
	$(MAKE) prep  1>prep.out 2>prep.out
# This should recompile Main.hs, because A in package p1 has changed
# and recompiling Main.hs will now fail.
	@(cd p2 && ./setup build -v0) || exit 0
	$(MAKE) clean 1>>clean.out 2>>clean.out

prep:
	cp p1/A1.hs p1/A.hs
	for i in p1 p2; do \
	   (cd $$i && \
		"$(TEST_HC)" --make -o setup Setup.hs && \
		./setup configure -v0 --with-compiler="$(TEST_HC)" --with-hc-pkg="$(GHC_PKG)" && \
		./setup build && \
		./setup register --inplace ); done
	cp p1/A2.hs p1/A.hs
	(cd p1 && ./setup build)

