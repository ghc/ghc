TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T7859:
	-'$(TEST_WRAPPER)' '$(RUNGHC)' -fdefer-type-errors T7859.hs

#The bug is that when reading from standard input, --ghc-arg= did not work correctly.
#In the buggy version, the following command would try and use mtl as a source file to
#compile.
T8601:
	-echo 'main = putStrLn "Hello World!"' | '$(TEST_WRAPPER)' '$(RUNGHC)' -f '$(TEST_HC)' -hide-package --ghc-arg=bytestring

T11247:
	# Should all work:
	'$(TEST_WRAPPER)' '$(RUNGHC)' T11247-hs
	'$(TEST_WRAPPER)' '$(RUNGHC)' T11247-hs.hs
	'$(TEST_WRAPPER)' '$(RUNGHC)' T11247-lhs
	'$(TEST_WRAPPER)' '$(RUNGHC)' T11247-lhs.lhs
	'$(TEST_WRAPPER)' '$(RUNGHC)' T11247-no-extension
	# Should print a "nice" error message that it can't find "foo." and
	# "foo.bar"
	-'$(TEST_WRAPPER)' '$(RUNGHC)' foo.
	-'$(TEST_WRAPPER)' '$(RUNGHC)' foo.bar

T17171a:
	'$(TEST_WRAPPER)' '$(RUNGHC)' --ghc-arg=-Wall T17171a.hs
T17171b:
	'$(TEST_WRAPPER)' '$(RUNGHC)' --ghc-arg=-Wall T17171b.hs

T-signals-child:
	-'$(TEST_WRAPPER)' '$(RUNGHC)' T-signals-child.hs --runghc '$(RUNGHC)'
