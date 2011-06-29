TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

LOCAL_PKGCONF=local.package.conf
PKG_NAME=simple-plugin

clean:
	rm -f $(LOCAL_PKGCONF)
	rm -rf dist
	rm -rf install

PREFIX := $(abspath install)
$(eval $(call canonicalise,PREFIX))

package:
	$(TEST_HC) --make -v0 -o setup Setup.hs
	
	echo "[]" >$(LOCAL_PKGCONF)
	
	./setup configure -v0 --prefix=$(PREFIX) --with-compiler="$(TEST_HC)" --with-hc-pkg="$(GHC_PKG)" --package-db=$(LOCAL_PKGCONF)
	./setup build -v0
	./setup install -v0
