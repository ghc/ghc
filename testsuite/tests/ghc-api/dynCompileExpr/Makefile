TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -f *.o *.hi

dynCompileExpr: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -v0 -package ghc dynCompileExpr
	./dynCompileExpr "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

.PHONY: clean dynCompileExpr

