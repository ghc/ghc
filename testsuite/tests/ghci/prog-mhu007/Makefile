TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

PKGCONF_FOO=local-foo.package.conf
PKGCONF_BAR=local-bar.package.conf
LOCAL_GHC_PKG_FOO = '$(GHC_PKG)' --no-user-package-db -f $(PKGCONF_FOO)
LOCAL_GHC_PKG_BAR = '$(GHC_PKG)' --no-user-package-db -f $(PKGCONF_BAR)

# Finds both packages in different unit databases
.PHONY: prog-mhu007
prog-mhu007:
	cd testpkg-foo && \
		'$(TEST_HC)' $(TEST_HC_OPTS) $(WAY_FLAGS) -hisuf=$(ghciWayExt) $(ghciWayFlags) \
			-v0 -fno-code -fwrite-interface -hidir dist-testpkg-foo-0.1.0.0 -this-unit-id testpkg-foo-0.1.0.0-XXX  \
			-i. Foo
	cd testpkg-bar && \
		'$(TEST_HC)' $(TEST_HC_OPTS) $(WAY_FLAGS) -hisuf=$(ghciWayExt) $(ghciWayFlags) \
			-v0 -fno-code -fwrite-interface -hidir dist-testpkg-bar-0.1.0.0 -this-unit-id testpkg-bar-0.1.0.0-XXX  \
			-i. Bar

	$(LOCAL_GHC_PKG_FOO) init $(PKGCONF_FOO) 2>/dev/null
	$(LOCAL_GHC_PKG_FOO) register --force testpkg-foo/testpkg-foo.pkg 2>/dev/null
	$(LOCAL_GHC_PKG_FOO) hide testpkg-foo
	$(LOCAL_GHC_PKG_FOO) list

	$(LOCAL_GHC_PKG_BAR) init $(PKGCONF_BAR) 2>/dev/null
	$(LOCAL_GHC_PKG_BAR) register --force testpkg-bar/testpkg-bar.pkg 2>/dev/null
	$(LOCAL_GHC_PKG_BAR) hide testpkg-bar
	$(LOCAL_GHC_PKG_BAR) list

	'$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) $(WAY_FLAGS) $(ghciWayFlags) \
		-no-user-package-db -fno-code -unit @unitA -unit @unitB < prog-mhu007.script
