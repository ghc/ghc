TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# -fforce-recomp makes lots of driver tests trivially pass, so we
# filter it out from $(TEST_HC_OPTS).
TEST_HC_OPTS_NO_RECOMP = $(filter-out -fforce-recomp,$(TEST_HC_OPTS))

# Recompilation tests

clean:
	rm -f *.o *.hi
	rm -f MyBool.hs Foo.hs Main.hs
	rm -f Main$(exeext)

# bug #7215

recomp012: clean
	echo 'module MyBool where data MyBool = MyFalse | MyTrue deriving Show' >MyBool.hs
	echo 'module Foo where import MyBool; foo = MyFalse' > Foo.hs
	echo 'import Foo; main = print foo' > Main.hs
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) -v0 -c -O2 MyBool.hs
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) -v0 -c -O2 Foo.hs
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) -v0 -O2 Main.hs
	./Main
	sleep 1
	echo 'module Foo where import MyBool; foo = MyTrue' > Foo.hs
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) -v0 -c -O2 Foo.hs
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) -v0 -O2 Main.hs
	./Main
