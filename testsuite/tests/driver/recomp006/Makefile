TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

OBJSUFFIX = .o

# Test that changing the fixity of a type synonym operator forces recompilation
# (see #3166).
recomp006:
	$(RM) A.hi A$(OBJSUFFIX)
	cp B1.hs B.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --make A.hs 1>out 2>err
	sleep 1
	cp B2.hs B.hs
	# This should fail
	if '$(TEST_HC)' $(TEST_HC_OPTS) --make A.hs; then false; fi
