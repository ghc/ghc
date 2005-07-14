TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

GHC_PKG=$(FPTOOLS_TOP)/ghc/utils/ghc-pkg/ghc-pkg-inplace

clean:
	rm -f setup a.out dist/build/testA
	rm -rf ,tmp dist ,tmp2
	find . -name "*.o" |xargs rm -f
	find . -name "*.hi" |xargs rm -f
	rm -fr install-tmp
	rm -fr install

setup : Setup.lhs
	$(TEST_HC) --make -o setup Setup.lhs

cabal01: setup
#	unregister if pakage 'test' already exists
	if $(GHC_PKG) --user list | grep test-; then \
	  $(GHC_PKG) --user unregister test; \
	fi

	./setup configure --prefix=`pwd`/install --with-compiler=$(TEST_HC) --with-hc-pkg=$(GHC_PKG) --enable-library-profiling
	./setup build
	./setup copy --copy-prefix=`pwd`/install-tmp
	ls install-tmp
#	install w/ register!
	./setup install --user
	ls install
	./setup sdist
	ls dist
	$(GHC_PKG) unregister --user test
