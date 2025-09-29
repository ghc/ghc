TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

test-bc: prepare
	'$(TEST_HC)' -fprefer-byte-code -fbyte-code-and-object-code -dynamic -unit @unita -unit @unitc

test-obj: prepare
	'$(TEST_HC)' -dynamic -unit @unita -unit @unitc

# make test should not yield
# <no location info>: error: unknown unit: dep1-1

prepare: clean
	'$(TEST_HC)' -c ./dep1/Dep1.hs -this-unit-id dep1-1 -dynamic -no-link -fPIC -osuf dyn_o -hisuf dyn_hi -o ./dep1/Dep1.dyn_o
	'$(TEST_HC)' -shared -dynamic -fPIC -o ./dep1/libHSdep1-1-ghc9.10.1.so ./dep1/Dep1.dyn_o
	'$(GHC_PKG)' --package-db dep1 recache

clean:
	$(RM) **/*.dyn_hi
	$(RM) **/*.dyn_o
	$(RM) **/*.hi
	$(RM) **/*.o
	$(RM) **/*.so
	$(RM) dep1/package.cache
	$(RM) dep1/package.cache.lock
	$(RM) libHSdep1-1-ghc9.10.1.so

