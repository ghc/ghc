TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Test 1: ghci -Ldir -lfoo
#   with dir/libfoo.a

.PHONY: ghcilink001
ghcilink001 :
	$(RM) -rf dir001
	mkdir dir001
	"$(TEST_HC)" -c f.c -o dir001/foo.o
	"$(AR)" cqs dir001/libfoo.a dir001/foo.o
	echo "test" | "$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) -Ldir001 -lfoo TestLink.hs

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
	echo "test" | "$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) -Ldir002 -lfoo TestLink.hs

# Test 3: ghci -package system-cxx-std-lib
#    where the C++ standard library is normally found in a directory private to
#    gcc, so only gcc can find it (see #5289)

.PHONY: ghcilink003
ghcilink003 :
	echo ":q" | "$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) -package system-cxx-std-lib

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
	echo 'key: test-XXX' >>$(PKG004)
	echo 'library-dirs: $${pkgroot}' >>$(PKG004)
	echo 'extra-libraries: foo' >>$(PKG004)
	'$(GHC_PKG)' init $(LOCAL_PKGCONF004)
	'$(GHC_PKG)' --no-user-package-db -f $(LOCAL_PKGCONF004) register $(PKG004) -v0
	#
	"$(TEST_HC)" -c f.c -o dir004/foo.o
	"$(AR)" cqs dir004/libfoo.a dir004/foo.o
	echo "test" | "$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) -package-db $(LOCAL_PKGCONF004) -package test TestLink.hs

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
	echo 'key: test-XXX' >>$(PKG005)
	echo 'library-dirs: $${pkgroot}' >>$(PKG005)
	echo 'extra-libraries: foo' >>$(PKG005)
	'$(GHC_PKG)' init $(LOCAL_PKGCONF005)
	'$(GHC_PKG)' --no-user-package-db -f $(LOCAL_PKGCONF005) register $(PKG005) -v0
	#
	"$(TEST_HC)" -c -dynamic f.c -o dir005/foo.o
	"$(TEST_HC)" -no-auto-link-packages -shared -o dir005/$(call DLL,foo) dir005/foo.o
	echo "test" | "$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) -package-db $(LOCAL_PKGCONF005) -package test TestLink.hs

# Test 6:
#   package P
#      build-depends: system-cxx-std-lib

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
	echo "key: test-XXX" >>$(PKG006)
	echo "build-depends: system-cxx-std-lib" >>$(PKG006)
	'$(GHC_PKG)' init $(LOCAL_PKGCONF006)
	'$(GHC_PKG)' --no-user-package-db -f $(LOCAL_PKGCONF006) register $(PKG006) -v0
	#
	echo ":q" | "$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) -package-db $(LOCAL_PKGCONF006) -package test

.PHONY: T3333
T3333:
	"$(TEST_HC)" -c T3333.c -o T3333.o
	echo "weak_test 10" | "$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) T3333.hs T3333.o

.PHONY: T11531
T11531:
	"$(TEST_HC)" -dynamic -fPIC -c T11531.c -o T11531.o
	- echo ":q" | "$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) T11531.o T11531.hs 2>&1 |  sed -e '/undefined symbol:/d' 1>&2

.PHONY: T14708
T14708:
	$(RM) -rf T14708scratch
	mkdir T14708scratch
	"$(TEST_HC)" -c add.c -o T14708scratch/add.o
	"$(AR)" cqs T14708scratch/libadd.a T14708scratch/add.o
	-"$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) -LT14708scratch -ladd T14708.hs

.PHONY: T15729
T15729:
	"$(TEST_HC)" -fPIC -c T15729.c -o bss.o
	echo "main" | "$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) bss.o T15729.hs

.PHONY: big-obj
big-obj:
	'$(TEST_CC)' -c -Wa,-mbig-obj big-obj-c.c -o big-obj-c.o
	echo "main" | '$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) big-obj-c.o big-obj.hs

.PHONY: T25155
T25155:
	'$(TEST_HC)' T25155_iserv_main.c T25155_iserv.hs -package ghci -no-hs-main -v0 -o T25155_iserv
	'$(TEST_HC)' -fexternal-interpreter -pgmi ./T25155_iserv -v0 T25155.hs
