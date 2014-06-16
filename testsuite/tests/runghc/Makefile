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
