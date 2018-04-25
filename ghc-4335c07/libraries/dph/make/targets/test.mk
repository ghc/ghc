
WAR_PACKAGES = \
	stm \
	parseargs \
	buildbox

dph-test/bin/war : $(shell find dph-test/framework -name "*.hs")
	@echo "* Build war test driver"
	@mkdir -p dph-test/bin
	@$(GHC_FRAMEWORK) \
		-threaded \
                $(patsubst %,-package %,$(WAR_PACKAGES)) \
		-idph-test/framework --make dph-test/framework/Main.hs -o dph-test/bin/war -threaded
	@echo

.PHONY : test-prims
test-prims : dph-test/bin/war
	@echo "* Running tests"
	@dph-test/bin/war -d dph-test/test
	@echo 
