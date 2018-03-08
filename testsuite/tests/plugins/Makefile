TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Each of the tests also exercise the various ways to bring plugins into scope

# -hide-all-plugin-packages + -plugin-package
.PHONY: plugins01
plugins01:
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) --make -v0 plugins01.hs -package-db simple-plugin/pkg.plugins01/local.package.conf -fplugin Simple.Plugin -fplugin-opt Simple.Plugin:Irrelevant_Option -hide-all-plugin-packages -plugin-package simple-plugin
	./plugins01

# -hide-all-packages + -plugin-package
.PHONY: plugins07
plugins07:
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) --make -v0 -O plugins07.hs -package-db rule-defining-plugin/pkg.plugins07/local.package.conf -hide-all-packages -package base -plugin-package rule-defining-plugin -fplugin=RuleDefiningPlugin
	./plugins07

.PHONY: plugins08
plugins08:
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) --make -v0 plugins08.hs -package-db simple-plugin/pkg.plugins08/local.package.conf
	./plugins08

# -package (should work for backwards compatibility)
.PHONY: T10420
T10420:
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) --make -v0 -O T10420.hs -package-db rule-defining-plugin/pkg.T10420/local.package.conf -hide-all-packages -package base -package rule-defining-plugin
	./T10420

.PHONY: T10294
T10294:
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) -c -v0 T10294.hs -package-db annotation-plugin/pkg.T10294/local.package.conf -package annotation-plugin -fplugin=SayAnnNames

.PHONY: T10294a
T10294a:
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) -c -v0 T10294a.hs -package-db annotation-plugin/pkg.T10294a/local.package.conf -package annotation-plugin -fplugin=SayAnnNames

.PHONY: frontend01
frontend01:
	$(RM) FrontendPlugin.hi FrontendPlugin.o frontend01 frontend01.hi frontend.o
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) -Wall -package ghc -c FrontendPlugin.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) --frontend FrontendPlugin -ffrontend-opt foo -ffrontend-opt bar frontend01
	./frontend01

# -hide-all-plugin-packages + -package (this should not work!)
.PHONY: T11244
T11244:
	! "$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) --make -v0 -O T11244.hs -package-db rule-defining-plugin/pkg.T11244/local.package.conf -hide-all-plugin-packages -package rule-defining-plugin -fplugin=RuleDefiningPlugin

.PHONY: T12567a
T12567a:
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) --make T12567a.hs -package-db simple-plugin/pkg.T12567a/local.package.conf -hide-all-plugin-packages -plugin-package simple-plugin 1>&2
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) --make T12567a.hs -package-db simple-plugin/pkg.T12567a/local.package.conf -hide-all-plugin-packages -plugin-package simple-plugin 2>&1 | grep "T12567a.hs, T12567a.o" 1>&2
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) --make T12567b.hs -package-db simple-plugin/pkg.T12567a/local.package.conf -hide-all-plugin-packages -plugin-package simple-plugin 1>&2

.PHONY: T14335
T14335:
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) -fexternal-interpreter --make -v0 plugins01.hs -package-db simple-plugin/pkg.plugins01/local.package.conf -fplugin Simple.Plugin -fplugin-opt Simple.Plugin:Irrelevant_Option -hide-all-plugin-packages -plugin-package simple-plugin
	./plugins01
