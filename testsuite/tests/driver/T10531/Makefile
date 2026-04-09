TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Test for #10531, a recompilation bug with '-main-is'.

T10531:
	'$(TEST_HC)' $(TEST_HC_OPTS) -Wall A.hs -main-is A.main -o a
	'$(TEST_HC)' $(TEST_HC_OPTS) -Wall B.hs -main-is B.main -o b
	'$(TEST_HC)' $(TEST_HC_OPTS) -Wall C.hs -o c
	rm -f A.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -Wall A.hs -main-is A.main -o a
	'$(TEST_HC)' $(TEST_HC_OPTS) -Wall B.hs -main-is B.main -o b
	./b$(exeext)
