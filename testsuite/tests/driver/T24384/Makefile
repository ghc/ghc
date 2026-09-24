TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk


PKGCONF_LOCAL=./db.package.conf
GHC_PKG_LOCAL='$(GHC_PKG)' --no-user-package-db -f $(PKGCONF_LOCAL)

T24384_prep:
	rm -r $(PKGCONF_LOCAL) || true
	$(GHC_PKG_LOCAL) init $(PKGCONF_LOCAL) 2>&1 >/dev/null

	cd dep-internal && \
		'$(TEST_HC)' $(TEST_HC_OPTS) $(WAY_FLAGS) -hisuf=$(ghciWayExt) $(ghciWayFlags) \
			-v0 -fno-code -fwrite-interface -hidir outdir -this-unit-id dep-internal-1-XXX  \
			-package base \
			-i. DepInternal
	$(GHC_PKG_LOCAL) register --force dep-internal/dep-internal.pkg 2>&1 >/dev/null

	cd dep-public && \
		'$(TEST_HC)' $(TEST_HC_OPTS) $(WAY_FLAGS) -hisuf=$(ghciWayExt) $(ghciWayFlags) \
			-v0 -fno-code -fwrite-interface -hidir outdir -this-unit-id dep-public-1-XXX  \
			-package base \
			-i. DepPublic
	$(GHC_PKG_LOCAL) register --force dep-public/dep-public.pkg 2>&1 >/dev/null

	cd dep && \
		'$(TEST_HC)' $(TEST_HC_OPTS) $(WAY_FLAGS) -hisuf=$(ghciWayExt) $(ghciWayFlags) \
			-v0 -fno-code -fwrite-interface -hidir outdir -this-unit-id dep-1-XXX \
			-package-db ../$(PKGCONF_LOCAL) -package-id dep-internal-1-XXX \
			-i. Dep
	$(GHC_PKG_LOCAL) register --force dep/dep.pkg 2>&1 >/dev/null

T24384a: T24384_prep
	'$(TEST_HC)' -M -dep-makefile T24384a.Makefile -include-pkg-deps -include-cpp-deps \
		-hide-all-packages -package base -package containers -package template-haskell \
		-package-db ./$(PKGCONF_LOCAL) -package-id dep-1-XXX -package-id dep-public-1-XXX \
		A.hs lib/B.hs C.hs D.hs E.hs

	cat T24384a.Makefile
