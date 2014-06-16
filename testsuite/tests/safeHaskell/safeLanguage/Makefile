TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Here we test that recompilation detection works correctly.
# (e.g # if the safe haskell flags change, recompilation should take place)

cleanSafeRecomp01:
	rm -rf SafeRecomp01.o SafeRecomp01.hi

# Just a single file
safeRecomp01:
	'$(MAKE)' cleanSafeRecomp01
	
	'$(TEST_HC)' -c SafeRecomp01.hs
	'$(TEST_HC)' --show-iface SafeRecomp01.hi | grep -E '^trusted:'
	
	'$(TEST_HC)' -XSafe -c SafeRecomp01.hs
	'$(TEST_HC)' --show-iface SafeRecomp01.hi | grep -E '^trusted:'
	
	'$(TEST_HC)' -XTrustworthy -c SafeRecomp01.hs
	'$(TEST_HC)' --show-iface SafeRecomp01.hi | grep -E '^trusted:'
	
	'$(TEST_HC)' -XUnsafe -c SafeRecomp01.hs
	'$(TEST_HC)' --show-iface SafeRecomp01.hi | grep -E '^trusted:'
	
	'$(TEST_HC)' -XUnsafe -c SafeRecomp01.hs
	'$(TEST_HC)' --show-iface SafeRecomp01.hi | grep -E '^trusted:'
	
	# what should happen when no safe haskell flag specified?
	# at moment we revert to 'no flags' so we recompile if previously
	# flags were specified. An alternate design would be to assume the
	# safe haskell flags from the old compile still apply but we
	# go with the previous design as that's the least suprise to a user.
	# See [SafeRecomp02] though.
	'$(TEST_HC)' -c SafeRecomp01.hs
	'$(TEST_HC)' --show-iface SafeRecomp01.hi | grep -E '^trusted:'

cleanSafeRecomp02:
	rm -rf SafeRecomp02 SafeRecomp02.o SafeRecomp02.hi SafeRecomp02_A.o SafeRecomp02_A.hi

# mutli module program
safeRecomp02:
	'$(MAKE)' cleanSafeRecomp02
	
	'$(TEST_HC)' -c SafeRecomp02_A.hs
	'$(TEST_HC)' --make SafeRecomp02.hs
	'$(TEST_HC)' --show-iface SafeRecomp02_A.hi | grep -E '^trusted:'
	'$(TEST_HC)' --show-iface SafeRecomp02.hi | grep -E '^trusted:'
	
	# Here we compile A with -XSafe but as we call --make with no flags,
	# A is recompiled in 'no flag' mode. Seems a little wrong. Maybe
	# we want that if a user is directly compiling a module we don't
	# use any old safe haskell flags, but if we are deciding to recompile
	# a module or not that was sucked up by --make, we consider no flags
	# to be equivalent to old flags?
	'$(TEST_HC)' -c -XSafe SafeRecomp02_A.hs
	'$(TEST_HC)' --make SafeRecomp02.hs
	'$(TEST_HC)' --show-iface SafeRecomp02_A.hi | grep -E '^trusted:'
	'$(TEST_HC)' --show-iface SafeRecomp02.hi | grep -E '^trusted:'
	
	# Assuming above design, we want A to be recompiled here I think as
	# -XSafe was explicitly specified as part of --make.
	'$(TEST_HC)' -c -XTrustworthy SafeRecomp02_A.hs
	'$(TEST_HC)' --make -XSafe SafeRecomp02.hs
	'$(TEST_HC)' --show-iface SafeRecomp02_A.hi | grep -E '^trusted:'
	'$(TEST_HC)' --show-iface SafeRecomp02.hi | grep -E '^trusted:'
	
	# Compiling manually so no recompilation checking
	'$(TEST_HC)' -c -XTrustworthy SafeRecomp02_A.hs
	'$(TEST_HC)' -c -XSafe SafeRecomp02.hs
	'$(TEST_HC)' -o SafeRecomp02 SafeRecomp02.o SafeRecomp02_A.o
	'$(TEST_HC)' --show-iface SafeRecomp02_A.hi | grep -E '^trusted:'
	'$(TEST_HC)' --show-iface SafeRecomp02.hi | grep -E '^trusted:'
	
	# manual compile followed by --make
	'$(TEST_HC)' -c -XTrustworthy SafeRecomp02_A.hs
	'$(TEST_HC)' -c -XSafe SafeRecomp02.hs
	'$(TEST_HC)' --make -o SafeRecomp02 SafeRecomp02.o SafeRecomp02_A.o
	'$(TEST_HC)' --show-iface SafeRecomp02_A.hi | grep -E '^trusted:'
	'$(TEST_HC)' --show-iface SafeRecomp02.hi | grep -E '^trusted:'

