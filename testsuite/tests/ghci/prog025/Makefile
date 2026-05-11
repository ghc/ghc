TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

PKGCONF01=local01.package.conf
LOCAL_GHC_PKG01 = '$(GHC_PKG)' --no-user-package-db -f $(PKGCONF01)

# When the user package databases are cleared, the GHCi prompt still needs to be able to find
# modules from home unit dependencies.
# Moreover, it needs to find the correct dependency unit, not just any.
.PHONY: prog025a
prog025a:
	cd testpkg && \
		'$(TEST_HC)' $(TEST_HC_OPTS) $(WAY_FLAGS) -hisuf=$(ghciWayExt) $(ghciWayFlags) \
			-v0 -fno-code -fwrite-interface -hidir dist-testpkg-0.1.0.0 -this-unit-id testpkg-0.1.0.0-XXX  \
			-i. Test
	cd testpkg && \
		'$(TEST_HC)' $(TEST_HC_OPTS) $(WAY_FLAGS) -hisuf=$(ghciWayExt) $(ghciWayFlags) \
			-v0 -fno-code -fwrite-interface -hidir dist-testpkg-0.2.0.0 -this-unit-id testpkg-0.2.0.0-XXX \
			-i. Test

	$(LOCAL_GHC_PKG01) init $(PKGCONF01) 2>/dev/null
	$(LOCAL_GHC_PKG01) register --force testpkg/testpkg-0.1.0.0.pkg 2>/dev/null
	$(LOCAL_GHC_PKG01) register --force testpkg/testpkg-0.2.0.0.pkg 2>/dev/null
	$(LOCAL_GHC_PKG01) hide testpkg-0.1.0.0
	$(LOCAL_GHC_PKG01) hide testpkg-0.2.0.0
	$(LOCAL_GHC_PKG01) list

	'$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) $(WAY_FLAGS) $(ghciWayFlags) \
		-no-user-package-db -fno-code -unit @unitA < prog025a.script

# Same as prog025a, but the home unit arguments are not provided using a response file
.PHONY: prog025b
prog025b:
	cd testpkg && \
		'$(TEST_HC)' $(TEST_HC_OPTS) $(WAY_FLAGS) -hisuf=$(ghciWayExt) $(ghciWayFlags) \
			-v0 -fno-code -fwrite-interface -hidir dist-testpkg-0.1.0.0 -this-unit-id testpkg-0.1.0.0-XXX  \
			-i. Test
	cd testpkg && \
		'$(TEST_HC)' $(TEST_HC_OPTS) $(WAY_FLAGS) -hisuf=$(ghciWayExt) $(ghciWayFlags) \
			-v0 -fno-code -fwrite-interface -hidir dist-testpkg-0.2.0.0 -this-unit-id testpkg-0.2.0.0-XXX \
			-i. Test

	$(LOCAL_GHC_PKG01) init $(PKGCONF01) 2>/dev/null
	$(LOCAL_GHC_PKG01) register --force testpkg/testpkg-0.1.0.0.pkg 2>/dev/null
	$(LOCAL_GHC_PKG01) register --force testpkg/testpkg-0.2.0.0.pkg 2>/dev/null
	$(LOCAL_GHC_PKG01) hide testpkg-0.1.0.0
	$(LOCAL_GHC_PKG01) hide testpkg-0.2.0.0
	$(LOCAL_GHC_PKG01) list

	# Uses response file for argument, doesn't use it as a -unit argument
	'$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) $(WAY_FLAGS) $(ghciWayFlags) \
		-no-user-package-db -fno-code \
		@unitA < prog025b.script
