TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

PKGCONF01=local01.package.conf
LOCAL_GHC_PKG01 = '$(GHC_PKG)' --no-user-package-db -f $(PKGCONF01)

STRIP_PKGROOT=sed 's/^pkgroot: .*$$/pkgroot: /'

# a bunch of tests for ghc-pkg
ghcpkg01 :
	@rm -rf $(PKGCONF01)
	$(LOCAL_GHC_PKG01) init $(PKGCONF01)
	$(LOCAL_GHC_PKG01) list
	$(LOCAL_GHC_PKG01) register --force test.pkg 2>/dev/null
	$(LOCAL_GHC_PKG01) describe testpkg         | $(STRIP_PKGROOT)
	$(LOCAL_GHC_PKG01) describe testpkg-1.2.3.4 | $(STRIP_PKGROOT)
	$(LOCAL_GHC_PKG01) field testpkg-1.2.3.4 import-dirs

	@: # test2.pkg is a later version of testpkg-1.2.3.4
	$(LOCAL_GHC_PKG01) register --force test2.pkg 2>/dev/null
	$(LOCAL_GHC_PKG01) list
	$(LOCAL_GHC_PKG01) describe testpkg-2.0 | $(STRIP_PKGROOT)
	$(LOCAL_GHC_PKG01) describe testpkg-*   | $(STRIP_PKGROOT)
	$(LOCAL_GHC_PKG01) field testpkg-* version

	@: # test hiding/exposing
	$(LOCAL_GHC_PKG01) hide testpkg-1.2.3.4
	$(LOCAL_GHC_PKG01) field testpkg-1.2.3.4 exposed | grep False
	$(LOCAL_GHC_PKG01) expose testpkg-2.0
	$(LOCAL_GHC_PKG01) field testpkg-2.0 exposed | grep True
	$(LOCAL_GHC_PKG01) hide testpkg-*
	$(LOCAL_GHC_PKG01) field testpkg-* exposed

	@: # test3.pkg  depends on testpkg-2.0
	$(LOCAL_GHC_PKG01) register test3.pkg

	@: # unregistering testpkg-2.0 should fail now, testpkg-3.0 depends on it
	if $(LOCAL_GHC_PKG01) unregister testpkg-2.0; then false; else true; fi

	$(LOCAL_GHC_PKG01) unregister testpkg-3.0

	$(LOCAL_GHC_PKG01) unregister testpkg-2.0
	$(LOCAL_GHC_PKG01) describe testpkg | $(STRIP_PKGROOT)
	$(LOCAL_GHC_PKG01) unregister testpkg-*
	$(LOCAL_GHC_PKG01) list

	@: # registering test3.pkg should fail now, its dependency has gone
	if $(LOCAL_GHC_PKG01) register test3.pkg; then false; else true; fi

PKGCONF03=local03.package.conf
LOCAL_GHC_PKG03 = '$(GHC_PKG)' --no-user-package-db -f $(PKGCONF03)

ghcpkg03 :
	@rm -rf $(PKGCONF03)
	$(LOCAL_GHC_PKG03) init $(PKGCONF03)
	@: # update should hide conflicting packages
	$(LOCAL_GHC_PKG03) register --force test.pkg
	$(LOCAL_GHC_PKG03) register --force test2.pkg

	@: # test4.pkg depends on testpkg-2.0, which conflicts with testpkg-1.2.3.4,
	@: # ghc-pkg will happily register it now (it didn't in GHC 6.4)
	$(LOCAL_GHC_PKG03) register test4.pkg
	$(LOCAL_GHC_PKG03) update test4.pkg

	@: # and another update of test.pkg will now hide dep-2.0
	$(LOCAL_GHC_PKG03) update --force test.pkg

PKGCONF04=local04.package.conf
LOCAL_GHC_PKG04 = '$(GHC_PKG)' --no-user-package-db -f $(PKGCONF04)

# Test that importing a module exposed by two packages reports a conflict
ghcpkg04 :
	rm -rf $(PKGCONF04)
	$(LOCAL_GHC_PKG04) init $(PKGCONF04)
	$(LOCAL_GHC_PKG04) register --force test.pkg >/dev/null 2>&1 
	$(LOCAL_GHC_PKG04) update --force test5.pkg >/dev/null 2>&1 
	$(LOCAL_GHC_PKG04) expose testpkg-1.2.3.4  >/dev/null 2>&1
	@: # testpkg-1.2.3.4 and newtestpkg-2.0 are both exposed now
	'$(TEST_HC)' $(TEST_HC_OPTS) -package-db $(PKGCONF04) -c ghcpkg04.hs || true

# Test stacking of package.confs (also #2441)
PKGCONF05a=local05a.package.conf
PKGCONF05b=local05b.package.conf
LOCAL_GHC_PKG05a = '$(GHC_PKG)' --no-user-package-db -f $(PKGCONF05a)
LOCAL_GHC_PKG05b = '$(GHC_PKG)' --no-user-package-db -f $(PKGCONF05a) -f $(PKGCONF05b)

ghcpkg05 :
	rm -rf $(PKGCONF05a) $(PKGCONF05b)
	$(LOCAL_GHC_PKG05a) init $(PKGCONF05a)
	$(LOCAL_GHC_PKG05b) init $(PKGCONF05b)
	$(LOCAL_GHC_PKG05a) register --force test2.pkg >/dev/null 2>&1 
	$(LOCAL_GHC_PKG05b) register --force test3.pkg >/dev/null 2>&1 
	$(LOCAL_GHC_PKG05b) check || true
	$(LOCAL_GHC_PKG05b) list
	$(LOCAL_GHC_PKG05b) expose testpkg-2.0
	$(LOCAL_GHC_PKG05b) hide   testpkg-2.0
	if $(LOCAL_GHC_PKG05b) unregister testpkg-2.0; then false; else true; fi
	$(LOCAL_GHC_PKG05b) unregister testpkg-3.0
	$(LOCAL_GHC_PKG05b) unregister testpkg-2.0

# Test for #2330: reject duplicate dependencies
PKGCONF06=local06.package.conf
LOCAL_GHC_PKG06 = '$(GHC_PKG)' --no-user-package-db -f $(PKGCONF06)

ghcpkg06 :
	rm -rf $(PKGCONF06)
	$(LOCAL_GHC_PKG06) init $(PKGCONF06)
	$(LOCAL_GHC_PKG06) register --force test.pkg >/dev/null 2>&1 
	if $(LOCAL_GHC_PKG06) register testdup.pkg; then false else true; fi
	$(LOCAL_GHC_PKG06) register --force testdup.pkg

PKGCONFT1750=localT1750.package.conf
LOCAL_GHC_PKGT1750 = '$(GHC_PKG)' --no-user-package-db -f $(PKGCONFT1750)

T1750:
	rm -rf $(PKGCONFT1750) T1750.hs T1750.o T1750.hi T1750.out
	$(LOCAL_GHC_PKGT1750) init $(PKGCONFT1750)
	$(LOCAL_GHC_PKGT1750) register --force T1750A.pkg >T1750.out 2>&1
	$(LOCAL_GHC_PKGT1750) register --force T1750B.pkg >T1750.out 2>&1
	GHC_PACKAGE_PATH=$(PKGCONFT1750) '$(GHC_PKG)' --no-user-package-db list
# GHC_PACKAGE_PATH trick is to make this work with 6.8.2 which doesn't have
# the patch "Change the command-line semantics for query commands" to
# ghc-pkg
	echo "main = return ()" >T1750.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -package-db $(PKGCONFT1750) -package T1750A T1750.hs || true

PKGCONFSHADOW1=localshadow1.package.conf
PKGCONFSHADOW2=localshadow2.package.conf
LOCAL_GHC_PKGSHADOW1 = '$(GHC_PKG)' --no-user-package-db -f $(PKGCONFSHADOW1)
LOCAL_GHC_PKGSHADOW2 = '$(GHC_PKG)' --no-user-package-db -f $(PKGCONFSHADOW2)
LOCAL_GHC_PKGSHADOW3 = '$(GHC_PKG)' --no-user-package-db -f $(PKGCONFSHADOW1) -f $(PKGCONFSHADOW2)

# Test package shadowing behaviour.
#
#     localshadow1.package.conf:  shadowdep-1-XXX <- shadow-1-XXX
#     localshadow2.package.conf:                     shadow-1-YYY
# 
# shadow-1-XXX will be shadowed by shadow-1-YYY, thus invalidating
# shadowdep-1-XXX.
shadow:
	rm -rf $(PKGCONFSHADOW1) $(PKGCONFSHADOW2) shadow.hs shadow.o shadow.hi shadow.out shadow.hs shadow.hi
	$(LOCAL_GHC_PKGSHADOW1) init $(PKGCONFSHADOW1)
	$(LOCAL_GHC_PKGSHADOW2) init $(PKGCONFSHADOW2)
	$(LOCAL_GHC_PKGSHADOW1) register -v0 --force shadow1.pkg
	$(LOCAL_GHC_PKGSHADOW1) register -v0 --force shadow2.pkg
	$(LOCAL_GHC_PKGSHADOW2) register -v0 --force shadow3.pkg
	$(LOCAL_GHC_PKGSHADOW3) list
	echo "main = return ()" >shadow.hs
# 
# In this test, shadow-1-XXX is shadowed by shadow-1-YYY, which causes
# shadowdep-1-XXX to be unavailable:
#
	@echo "should FAIL:"
	'$(TEST_HC)' $(TEST_HC_OPTS) -package-db $(PKGCONFSHADOW1) -package-db $(PKGCONFSHADOW2) -package shadowdep -c shadow.hs -fno-code || true
# 
# Reversing the order of the package.conf files should fix the problem:
#
	@echo "should SUCCEED:"
	'$(TEST_HC)' $(TEST_HC_OPTS) -package-db $(PKGCONFSHADOW2) -package-db $(PKGCONFSHADOW1) -package shadowdep -c shadow.hs -fno-code || true
#
# We can also fix the problem by using an explicit -package-id flag to
# specify a package we really want to use:
#
	@echo "should SUCCEED:"
	'$(TEST_HC)' $(TEST_HC_OPTS) -package-db $(PKGCONFSHADOW1) -package-db $(PKGCONFSHADOW2) -package-id shadowdep-1-XXX -c shadow.hs -fno-code

# If we pass --global, we should ignore instances in the user database
T5442a:
	@rm -rf package.conf.T5442a.global package.conf.T5442a.user
	'$(GHC_PKG)' init package.conf.T5442a.global
	'$(GHC_PKG)' init package.conf.T5442a.user
	'$(GHC_PKG)' -f package.conf.T5442a.global register --force-files test.pkg 2>/dev/null
	'$(GHC_PKG)' -f package.conf.T5442a.user register --force-files test.pkg 2>/dev/null
	'$(GHC_PKG)' --global-package-db=package.conf.T5442a.global --user-package-db=package.conf.T5442a.user --global unregister testpkg
	@echo "global (should be empty):"
	'$(GHC_PKG)' -f package.conf.T5442a.global list --simple-output
	@echo "user:"
	'$(GHC_PKG)' -f package.conf.T5442a.user list --simple-output

# If we pass --user, we should ignore instances in the global database
T5442b:
	@rm -rf package.conf.T5442b.global package.conf.T5442b.user
	'$(GHC_PKG)' init package.conf.T5442b.global
	'$(GHC_PKG)' init package.conf.T5442b.user
	'$(GHC_PKG)' -f package.conf.T5442b.global register --force-files test.pkg 2>/dev/null
	! '$(GHC_PKG)' --global-package-db=package.conf.T5442b.global --user-package-db=package.conf.T5442b.user --user unregister testpkg
	@echo "global (should have testpkg):"
	'$(GHC_PKG)' -f package.conf.T5442b.global list --simple-output

# If we pass -f, we should ignore the user and global databases
T5442c:
	@rm -rf package.conf.T5442c.global package.conf.T5442c.user package.conf.T5442c.extra
	'$(GHC_PKG)' init package.conf.T5442c.global
	'$(GHC_PKG)' init package.conf.T5442c.user
	'$(GHC_PKG)' init package.conf.T5442c.extra
	'$(GHC_PKG)' -f package.conf.T5442c.global register --force-files test.pkg 2>/dev/null
	'$(GHC_PKG)' -f package.conf.T5442c.user register --force-files test.pkg 2>/dev/null
	! '$(GHC_PKG)' --global-package-db=package.conf.T5442c.global --user-package-db=package.conf.T5442c.user -f package.conf.T5442c.extra unregister testpkg
	@echo "global (should have testpkg):"
	'$(GHC_PKG)' -f package.conf.T5442c.global list --simple-output
	@echo "use (should have testpkg):"
	'$(GHC_PKG)' -f package.conf.T5442c.user list --simple-output

# If we pass --global and -f, we remove from the global database, but
# warn about possible breakage in the full package stack
T5442d:
	@rm -rf package.conf.T5442d.global package.conf.T5442d.user package.conf.T5442d.extra
	'$(GHC_PKG)' init package.conf.T5442d.global
	'$(GHC_PKG)' init package.conf.T5442d.user
	'$(GHC_PKG)' init package.conf.T5442d.extra
	'$(GHC_PKG)' -f package.conf.T5442d.global register --force-files shadow1.pkg 2>/dev/null
	'$(GHC_PKG)' -f package.conf.T5442d.user register --force-files shadow3.pkg 2>/dev/null
	'$(GHC_PKG)' --global-package-db=package.conf.T5442d.global -f package.conf.T5442d.extra register --force-files shadow2.pkg 2>/dev/null
	'$(GHC_PKG)' --global-package-db=package.conf.T5442d.global --user-package-db=package.conf.T5442d.user -f package.conf.T5442d.extra --global unregister shadow --force
	@echo "global (should be empty):"
	'$(GHC_PKG)' -f package.conf.T5442d.global list --simple-output
	@echo "user:"
	'$(GHC_PKG)' -f package.conf.T5442d.user list --simple-output
	@echo "extra:"
	'$(GHC_PKG)' -f package.conf.T5442d.extra list --simple-output

# -----------------------------------------------------------------------------
# Try piping the output of "ghc-pkg describe" into "ghc-pkg update" for
# every package we know about.  This is for testing the pretty printing/parsing
# of InstalledPackageInfo.

PACKAGE_CONF_ghcpkg02 = package.conf.ghcpkg02
GHC_PKG_ghcpkg02 = '$(GHC_PKG)' --no-user-package-db --global-package-db=$(PACKAGE_CONF_ghcpkg02)

ghcpkg02:
	rm -rf $(PACKAGE_CONF_ghcpkg02)
	$(GHC_PKG_ghcpkg02) init $(PACKAGE_CONF_ghcpkg02)
	set -e; \
	for i in `$(GHC_PKG) list --global --simple-output -v0`; do \
		echo Updating $$i; \
		$(GHC_PKG) describe --global $$i | $(GHC_PKG_ghcpkg02) update --global --force -; \
	done
