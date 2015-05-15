TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: plugins01 plugins07

plugins01:
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) --make -v0 plugins01.hs -package-db simple-plugin/pkg.plugins01/local.package.conf -fplugin Simple.Plugin -fplugin-opt Simple.Plugin:Irrelevant_Option -package simple-plugin
	./plugins01

plugins07:
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) --make -v0 -O plugins07.hs -package-db rule-defining-plugin/pkg.plugins07/local.package.conf -package rule-defining-plugin
	./plugins07
