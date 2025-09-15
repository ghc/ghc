TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

make-prim:
	# build once to test the substitution of the virtual interface in --make
	# mode with codegen
	"$(TEST_HC)" $(TEST_HC_OPTS) --make Test.hs -this-unit-id ghc-internal -hide-all-packages -dno-typeable-binds -v0
	# build a different module (Test2) in --make mode to test the reloading
	# of the GHC.Internal.Prim interface
	"$(TEST_HC)" $(TEST_HC_OPTS) --make Test2.hs -this-unit-id ghc-internal -hide-all-packages -dno-typeable-binds -v0
