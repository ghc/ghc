TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Original comments relating to GHC 6.4:
# This test caused a panic in GHC 6.4, but in reality it is user
# error, caused by compiling separate modules in the program using
# different search paths.  GHC gets confused when it can't find a
# module mentioned as a dependency in another interface file.

# Update for GHC 6.6:
# This test now succeeds, because A.hi records the fact that B.C
# came from the package test-1.0, so there's no conflict.

# If the package format changes, we might have to re-generate pkg.conf
# using the supplied Cabal configuration.

rn.prog006:
	$(TEST_HC) --make -package-name test-1.0 B.C -no-recomp -v0
	sed "s@__IMPORT__@`pwd`@" <pkg.conf.in >pkg.conf
	$(TEST_HC) -c -package-conf pkg.conf -package test -no-recomp A.hs -i
# The -i clears the search path, so A.hs will find B.C from package test
#
	-$(TEST_HC) -c -package-conf pkg.conf -package test -no-recomp Main.hs
# No -i when compiling Main, so a from-scratch search would find a home-pkg module B.C
# However, A.hi remembers that B.C came from package test, so all is ok.

