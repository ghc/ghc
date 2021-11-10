TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

UserTickyCounter :
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) --make -v0 -package-db user-ticky-counter-plugin/pkg/local.package.conf -fplugin=UserTickyCounterPlugin -ticky UserTickyCounter
	./UserTickyCounter +RTS -rticky
	cat ticky
