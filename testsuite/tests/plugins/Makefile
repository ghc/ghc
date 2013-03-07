TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: plugins01

plugins01:
	"$(TEST_HC)" $(HC_OPTS) -fforce-recomp --make -v0 plugins01.hs -package-db simple-plugin/pkg.plugins01/local.package.conf -fplugin Simple.Plugin -fplugin-opt Simple.Plugin:Irrelevant_Option -package simple-plugin
	./plugins01

