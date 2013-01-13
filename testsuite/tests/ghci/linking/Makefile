TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

LOCAL_GHC_PKG = '$(GHC_PKG)' --no-user-package-db -f $(LOCAL_PKGCONF)

# Test 1: ghci -Ldir -lfoo
#   with dir/libfoo.a

.PHONY: ghcilink001
ghcilink001 :
	$(RM) -rf dir001
	mkdir dir001
	"$(TEST_HC)" -c f.c -o dir001/foo.o
	"$(AR)" cqs dir001/libfoo.a dir001/foo.o
	echo "test" | "$(TEST_HC)" --interactive -ignore-dot-ghci -v0 -Ldir001 -lfoo TestLink.hs

# Test 2: ghci -Ldir -lfoo
#   with dir/libfoo.so

ifeq "$(WINDOWS)" "YES"
DLL = $1.dll
else ifeq "$(DARWIN)" "YES"
DLL = lib$1.dylib
else
DLL = lib$1.so
endif

.PHONY: ghcilink002
ghcilink002 :
	$(RM) -rf dir002
	mkdir dir002
	"$(TEST_HC)" -c -dynamic f.c -o dir002/foo.o
	"$(TEST_HC)" -no-auto-link-packages -shared -v0 -o dir002/$(call DLL,foo) dir002/foo.o
	echo "test" | "$(TEST_HC)" --interactive -ignore-dot-ghci -v0 -Ldir002 -lfoo TestLink.hs

# Test 3: ghci -lstdc++
#    where libstdc++.so is normally found in a directory private to gcc,
#    so only gcc can find it (see #5289)

.PHONY: ghcilink003
ghcilink003 :
	echo ":q" | "$(TEST_HC)" --interactive -ignore-dot-ghci -v0 -lstdc++

# Test 4: 
#   package P
#      library-dirs: `pwd`/dir004
#      extra-libraries: foo
#   with 
#    dir004/libfoo.a

LOCAL_PKGCONF004=dir004/local.package.conf
PKG004=dir004/pkg.conf

ghcilink004 :
	$(RM) -rf dir004
	mkdir dir004
	#
	rm -f $(PKG004)
	echo 'name: test' >>$(PKG004)
	echo 'version: 1.0' >>$(PKG004)
	echo 'id: test-XXX' >>$(PKG004)
	echo 'library-dirs: $${pkgroot}' >>$(PKG004)
	echo 'extra-libraries: foo' >>$(PKG004)
	echo '[]' >$(LOCAL_PKGCONF004)
	'$(GHC_PKG)' --no-user-package-db -f $(LOCAL_PKGCONF004) register $(PKG004) -v0
	#
	"$(TEST_HC)" -c f.c -o dir004/foo.o
	"$(AR)" cqs dir004/libfoo.a dir004/foo.o
	echo "test" | "$(TEST_HC)" --interactive -ignore-dot-ghci -v0 -package-db $(LOCAL_PKGCONF004) -package test TestLink.hs

# Test 5: 
#   package P
#      library-dirs: `pwd`/dir005
#      extra-libraries: foo
#   with 
#    dir005/libfoo.so

LOCAL_PKGCONF005=dir005/ghcilink005.package.conf
PKG005=dir005/pkg.conf

ghcilink005 :
	$(RM) -rf dir005
	mkdir dir005
	#
	rm -f $(PKG005)
	echo 'name: test' >>$(PKG005)
	echo 'version: 1.0' >>$(PKG005)
	echo 'id: test-XXX' >>$(PKG005)
	echo 'library-dirs: $${pkgroot}' >>$(PKG005)
	echo 'extra-libraries: foo' >>$(PKG005)
	echo '[]' >$(LOCAL_PKGCONF005)
	'$(GHC_PKG)' --no-user-package-db -f $(LOCAL_PKGCONF005) register $(PKG005) -v0
	#
	"$(TEST_HC)" -c -dynamic f.c -o dir005/foo.o
	"$(TEST_HC)" -no-auto-link-packages -shared -o dir005/$(call DLL,foo) dir005/foo.o
	echo "test" | "$(TEST_HC)" --interactive -ignore-dot-ghci -v0 -package-db $(LOCAL_PKGCONF005) -package test TestLink.hs

# Test 6: 
#   package P
#      extra-libraries: stdc++

LOCAL_PKGCONF006=dir006/ghcilink006.package.conf
PKG006=dir006/pkg.conf

ghcilink006 :
	$(RM) -rf dir006
	mkdir dir006
	#
	rm -f $(PKG006)
	echo "name: test" >>$(PKG006)
	echo "version: 1.0" >>$(PKG006)
	echo "id: test-XXX" >>$(PKG006)
	echo "extra-libraries: stdc++" >>$(PKG006)
	echo "[]" >$(LOCAL_PKGCONF006)
	'$(GHC_PKG)' --no-user-package-db -f $(LOCAL_PKGCONF006) register $(PKG006) -v0
	#
	echo ":q" | "$(TEST_HC)" --interactive -ignore-dot-ghci -v0 -package-db $(LOCAL_PKGCONF006) -package test

.PHONY: T3333
T3333:
	"$(TEST_HC)" -c T3333.c -o T3333.o
	echo "weak_test 10" | "$(TEST_HC)" --interactive -ignore-dot-ghci -v0 T3333.hs T3333.o
