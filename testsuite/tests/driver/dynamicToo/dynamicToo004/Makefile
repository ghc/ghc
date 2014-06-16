TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

checkExists = [ -f $1 ] || echo $1 missing

LOCAL_PKGCONF=local.package.conf

.PHONY: clean
clean:
	rm -f Setup.o Setup.hi Setup Setup.exe
	rm -rf $(LOCAL_PKGCONF)
	rm -rf pkg1/dist
	rm -rf pkg1dyn/dist
	rm -rf pkg2/dist
	rm -f prog.o     prog.hi     progstatic  progstatic.exe
	rm -f prog.dyn_o prog.dyn_hi progdynamic progdynamic.exe

.PHONY: dynamicToo004
dynamicToo004:
	$(MAKE) clean

	"$(GHC_PKG)" init $(LOCAL_PKGCONF)
	"$(TEST_HC)" -v0 --make Setup.hs

# First make the vanilla pkg1
	cd pkg1 && ../Setup configure -v0 --with-compiler="$(TEST_HC)" --with-hc-pkg="$(GHC_PKG)" --package-db=../$(LOCAL_PKGCONF) --enable-library-vanilla --disable-shared
	cd pkg1 && ../Setup build
	cd pkg1 && ../Setup register --inplace

# Then the dynamic pkg1. This has different code in A.hs, so we get
# a different hash.
	cd pkg1dyn && ../Setup configure -v0 --with-compiler="$(TEST_HC)" --with-hc-pkg="$(GHC_PKG)" --package-db=../$(LOCAL_PKGCONF) --disable-library-vanilla --enable-shared
	cd pkg1dyn && ../Setup build

# Now merge the dynamic outputs into the registered directory
	cp pkg1dyn/dist/build/A.dyn_hi   pkg1/dist/build/
	cp pkg1dyn/dist/build/A.dyn_o    pkg1/dist/build/
	cp pkg1dyn/dist/build/libHSpkg1* pkg1/dist/build/

# Next compile pkg2 both ways, which will use -dynamic-too
	cd pkg2 && ../Setup configure -v0 --with-compiler="$(TEST_HC)" --with-hc-pkg="$(GHC_PKG)" --package-db=../$(LOCAL_PKGCONF) --enable-library-vanilla --enable-shared
	cd pkg2 && ../Setup build
	cd pkg2 && ../Setup register --inplace

# And then compile a program using the library both ways
	"$(TEST_HC)" -package-db $(LOCAL_PKGCONF) --make prog -o progstatic
	"$(TEST_HC)" -package-db $(LOCAL_PKGCONF) --make prog -o progdynamic -dynamic -osuf dyn_o -hisuf dyn_hi

# Both should run, giving their respective outputs
	echo static
	./progstatic
	echo dynamic
	./progdynamic

