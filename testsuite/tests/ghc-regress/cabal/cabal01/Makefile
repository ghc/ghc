TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -rf setup a.out dist/build/testA
	rm -rf ,tmp dist ,tmp2
	find . -name "*.o" |xargs rm -f
	find . -name "*.hi" |xargs rm -f
	rm -fr install-tmp
	rm -fr install
	rm -f .setup-config .installed-pkg-config

setup : Setup.lhs
	$(TEST_HC) --make -o setup Setup.lhs

cabal01: setup
#	unregister if pakage 'test' already exists
	if $(GHC_PKG_INPLACE) --user list | grep test-; then \
	  $(GHC_PKG_INPLACE) --user unregister test; \
	fi

	./setup configure --prefix=`pwd`/install --with-compiler=$(TEST_HC) --with-hc-pkg=$(GHC_PKG_INPLACE) --enable-library-profiling
	./setup build
	./setup copy --copy-prefix=`pwd`/install-tmp
	ls install-tmp
#	install w/ register!
	./setup install --user
	ls install
	./setup sdist
	ls dist
	$(GHC_PKG_INPLACE) unregister --user test
