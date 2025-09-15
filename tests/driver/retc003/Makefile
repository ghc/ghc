TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

OBJSUFFIX = .o

# Test that adding a new module that shadows a package module causes
# recompilation.  Part of bug #1372.
retc003:
	$(RM) A.hi A$(OBJSUFFIX) out
	$(RM) -rf Data
	mkdir Data
	'$(TEST_HC)' $(TEST_HC_OPTS) -fno-code -fwrite-interface -c A.hs
	echo 'Middle'
	'$(TEST_HC)' $(TEST_HC_OPTS) -fno-code -fwrite-interface -c A.hs
	echo 'End'
	echo "module Data.Char where" > Data/Char.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -fno-code -fwrite-interface -c Data/Char.hs
	# Should now recompile A.hs, because Char is now a home module:
	'$(TEST_HC)' $(TEST_HC_OPTS) -fno-code -fwrite-interface -c A.hs
