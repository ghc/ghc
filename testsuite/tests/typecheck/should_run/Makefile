TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: AppendSymbolDeductionReadback
AppendSymbolDeductionReadback:
	@mkdir -p producer consumer
	@cp AppendSymbolDeductionProvider.hs producer/
	@cd producer && '$(TEST_HC)' $(TEST_HC_OPTS) -v0 -O2 -dcore-lint -dstg-lint -c AppendSymbolDeductionProvider.hs
	@cp producer/AppendSymbolDeductionProvider.hi AppendSymbolDeductionReadback.hs consumer/
	@cd consumer && '$(TEST_HC)' $(TEST_HC_OPTS) -v0 -O2 -dcore-lint -dstg-lint -i -i. -c AppendSymbolDeductionReadback.hs
	@cd consumer && '$(TEST_HC)' $(TEST_HC_OPTS) -v0 AppendSymbolDeductionReadback.o ../producer/AppendSymbolDeductionProvider.o -o check$(exeext)
	@./consumer/check$(exeext)
