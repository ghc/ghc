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

.PHONY: plugins09
plugins09:
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) --make -v0 plugins09.hs -package-db simple-plugin/pkg.plugins09/local.package.conf -fplugin Simple.SourcePlugin -fplugin-opt Simple.SourcePlugin:a -fplugin-opt Simple.SourcePlugin:b -plugin-package simple-plugin

.PHONY: plugins10
plugins10:
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) --make -v0 plugins10.hs QuasiQuotation.hs -package-db simple-plugin/pkg.plugins10/local.package.conf -fplugin Simple.SourcePlugin -plugin-package simple-plugin

.PHONY: plugins11
plugins11:
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) --make -v0 plugins11.hs -package-db simple-plugin/pkg.plugins11/local.package.conf -plugin-package simple-plugin

.PHONY: plugins12
plugins12:
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) --make -v0 plugins12.hs -package-db simple-plugin/pkg.plugins12/local.package.conf -plugin-package simple-plugin

.PHONY: plugins13
plugins13:
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) --make -v0 plugins13.hs PluginFilteredExport.hs -package-db simple-plugin/pkg.plugins13/local.package.conf -plugin-package simple-plugin

.PHONY: plugins14
plugins14:
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) --make -v0 plugins14.hs -package-db simple-plugin/pkg.plugins14/local.package.conf -plugin-package simple-plugin

.PHONY: plugins15
plugins15:
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) --make -v0 plugins15.hs MetaRemoveHelper.hs -package-db simple-plugin/pkg.plugins15/local.package.conf -plugin-package simple-plugin

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
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) --make T12567a.hs -package-db simple-plugin/pkg.T12567a/local.package.conf -hide-all-plugin-packages -plugin-package simple-plugin 1>&2
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) --make T12567b.hs -package-db simple-plugin/pkg.T12567a/local.package.conf -hide-all-plugin-packages -plugin-package simple-plugin 1>&2

.PHONY: T14335
T14335:
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) -fexternal-interpreter --make -v0 plugins01.hs -package-db simple-plugin/pkg.plugins01/local.package.conf -fplugin Simple.Plugin -fplugin-opt Simple.Plugin:Irrelevant_Option -hide-all-plugin-packages -plugin-package simple-plugin
	./plugins01

# Shouldn't recompile the module
.PHONY: plugin-recomp-pure
plugin-recomp-pure:
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) plugin-recomp-test.hs -package-db plugin-recomp/pkg.plugins01/local.package.conf -fplugin PurePlugin
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) plugin-recomp-test.hs -package-db plugin-recomp/pkg.plugins01/local.package.conf -fplugin PurePlugin

# Should recompile the module
.PHONY: plugin-recomp-impure
plugin-recomp-impure:
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) plugin-recomp-test.hs -package-db plugin-recomp/pkg.plugins01/local.package.conf -fplugin ImpurePlugin
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) plugin-recomp-test.hs -package-db plugin-recomp/pkg.plugins01/local.package.conf -fplugin ImpurePlugin

# Should not recompile the module the first time but should the second time
.PHONY: plugin-recomp-flags
plugin-recomp-flags:
	"$(TEST_HC)" $(TEST_HC_OPTS)  $(ghcPluginWayFlags) plugin-recomp-test.hs -package-db plugin-recomp/pkg.plugins01/local.package.conf -fplugin FingerprintPlugin -fplugin-opt FingerprintPlugin:0
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) plugin-recomp-test.hs -package-db plugin-recomp/pkg.plugins01/local.package.conf -fplugin FingerprintPlugin -fplugin-opt FingerprintPlugin:0
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) plugin-recomp-test.hs -package-db plugin-recomp/pkg.plugins01/local.package.conf -fplugin FingerprintPlugin -fplugin-opt FingerprintPlugin:1
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) plugin-recomp-test.hs -package-db plugin-recomp/pkg.plugins01/local.package.conf -fplugin FingerprintPlugin -fplugin-opt FingerprintPlugin:1

# Should recompile the module because the plugin changed
.PHONY: plugin-recomp-change
plugin-recomp-change:
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) -v0 plugin-recomp-test.hs -package-db plugin-recomp/pkg.plugins01/local.package.conf -fplugin PurePlugin
	"$(MAKE)" -s --no-print-directory -C plugin-recomp package.plugins01 TOP=$(TOP) RUN=-DRUN2
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) -v0 plugin-recomp-test.hs -package-db plugin-recomp/pkg.plugins01/local.package.conf -fplugin PurePlugin

# Should recompile the module because the plugin changed, test for 15492
.PHONY: plugin-recomp-change-prof
plugin-recomp-change-prof:
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) -v0 plugin-recomp-test.hs -package-db plugin-recomp/pkg.plugins01/local.package.conf -fplugin PurePlugin
	"$(TEST_HC)" $(TEST_HC_OPTS) -prof -osuf p_o -hisuf p_hi -v0 plugin-recomp-test.hs -package-db plugin-recomp/pkg.plugins01/local.package.conf -fplugin PurePlugin
	"$(MAKE)" -s --no-print-directory -C plugin-recomp package.plugins01 TOP=$(TOP) RUN=-DRUN2
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) -v0 plugin-recomp-test.hs -package-db plugin-recomp/pkg.plugins01/local.package.conf -fplugin PurePlugin
	"$(TEST_HC)" $(TEST_HC_OPTS) -prof -osuf p_o -hisuf p_hi -v0 plugin-recomp-test.hs -package-db plugin-recomp/pkg.plugins01/local.package.conf -fplugin PurePlugin

.PHONY: T16104
T16104:
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) -v0 T16104.hs -package-db T16104-plugin/pkg.T16104-plugin/local.package.conf

.PHONY: T16260
T16260:
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) -v0 T16260.hs -package-db simple-plugin/pkg.T16260/local.package.conf -fplugin Simple.TrustworthyPlugin
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) -v0 T16260.hs -package-db simple-plugin/pkg.T16260/local.package.conf -fplugin Simple.TrustworthyPlugin -fplugin-trustworthy

.PHONY: HoleFitPlugin
HoleFitPlugin:
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghcPluginWayFlags) -v0 HoleFitPlugin.hs -package-db hole-fit-plugin/pkg.hole-fit-plugin/local.package.conf
