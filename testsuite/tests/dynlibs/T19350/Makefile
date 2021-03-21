TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

LOCAL_PKGCONF=local.package.conf

T19350:
	echo "Building libhello..."
	'$(TEST_HC)' -fPIC -c clib/lib.c -o clib/lib.o
	'$(TEST_HC)' -shared -no-hs-main clib/lib.o -o clib/libhello$(dllext)
	
	rm -Rf $(LOCAL_PKGCONF)
	"$(GHC_PKG)" init $(LOCAL_PKGCONF)
	
	echo "Building T19350-lib..."
	cd lib && '$(TEST_HC)' -package Cabal Setup.hs
	x="$$(pwd)//clib" && cd lib && ./Setup configure -v0 --extra-lib-dirs="$$x" --extra-lib-dirs="$$x-install" --with-compiler="$(TEST_HC)" --with-hc-pkg="$(GHC_PKG)" --package-db=../$(LOCAL_PKGCONF) --disable-library-vanilla --enable-shared
	cd lib && ./Setup build
	cd lib && ./Setup register --inplace
	
	# Move libhello from its original place to ensure it isn't found via RPATH
	mkdir -p clib-install
	mv clib/libhello$(dllext) clib-install
	
	echo "Testing..."
	'$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) -package-db '$(LOCAL_PKGCONF)' -package T19350-lib  -interactive < T19350.script
