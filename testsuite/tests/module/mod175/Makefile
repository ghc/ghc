TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -f *.o
	rm -f *.hi
	rm -f test test2

# Test for trac bug #437 (Test2 needs to be recompiled in the second GHC
# invocation as the main-is flag now applies to it).

mod175:
	$(MAKE) clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make -main-is Test.main  Test.hs  -o test
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make -main-is Test2.main Test2.hs -o test2
	./test
	./test2

