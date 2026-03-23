TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Test that compiling X.hs without implicit Prelude does not recompile X the
# second time or third time. In !15899, X.hs was recompiled because of an
# incorrect "GHC.Essentials package changed"
clean:
	rm -f *.o *.hi

T27013c: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) --make X.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --make X.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --make X.hs
