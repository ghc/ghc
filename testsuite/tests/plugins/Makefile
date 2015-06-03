TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: plugins01
plugins01:
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) --make -v0 plugins01.hs -package-db simple-plugin/pkg.plugins01/local.package.conf -fplugin Simple.Plugin -fplugin-opt Simple.Plugin:Irrelevant_Option -package simple-plugin
	./plugins01

.PHONY: plugins07
plugins07:
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) --make -v0 -O plugins07.hs -package-db rule-defining-plugin/pkg.plugins07/local.package.conf -package rule-defining-plugin -fplugin=RuleDefiningPlugin
	./plugins07

.PHONY: T10420
T10420:
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) --make -v0 -O T10420.hs -package-db rule-defining-plugin/pkg.T10420/local.package.conf -package rule-defining-plugin
	./T10420

.PHONY: T10294
T10294:
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) -c -v0 T10294.hs -package-db annotation-plugin/pkg.T10294/local.package.conf -package annotation-plugin -fplugin=SayAnnNames

.PHONY: T10294a
T10294a:
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) -c -v0 T10294a.hs -package-db annotation-plugin/pkg.T10294a/local.package.conf -package annotation-plugin -fplugin=SayAnnNames
