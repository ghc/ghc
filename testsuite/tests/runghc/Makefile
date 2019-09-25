TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T7859:
	-'$(RUNGHC)' -fdefer-type-errors T7859.hs

#The bug is that when reading from standard input, --ghc-arg= did not work correctly.
#In the buggy version, the following command would try and use mtl as a source file to
#compile.
T8601:
	-echo 'main = putStrLn "Hello World!"' | '$(RUNGHC)' -f '$(TEST_HC)' -hide-package --ghc-arg=bytestring

T11247:
	# Should all work:
	'$(RUNGHC)' T11247-hs
	'$(RUNGHC)' T11247-hs.hs
	'$(RUNGHC)' T11247-lhs
	'$(RUNGHC)' T11247-lhs.lhs
	'$(RUNGHC)' T11247-no-extension
	# Should print a "nice" error message that it can't find "foo." and
	# "foo.bar"
	-'$(RUNGHC)' foo.
	-'$(RUNGHC)' foo.bar

T17171a:
	'$(RUNGHC)' --ghc-arg=-Wall T17171a.hs
T17171b:
	'$(RUNGHC)' --ghc-arg=-Wall T17171b.hs

T-signals-child:
	-'$(RUNGHC)' T-signals-child.hs --runghc '$(RUNGHC)'
