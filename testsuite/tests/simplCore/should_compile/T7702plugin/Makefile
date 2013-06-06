TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean.%:
	rm -rf pkg.$*

HERE := $(abspath .)
$(eval $(call canonicalise,HERE))

package.%:
	$(MAKE) clean.$*
	mkdir pkg.$*
	"$(TEST_HC)" -outputdir pkg.$* --make -v0 -o pkg.$*/setup Setup.hs

	echo "[]" > pkg.$*/local.package.conf

	pkg.$*/setup configure --distdir pkg.$*/dist -v0 $(CABAL_PLUGIN_BUILD) --prefix="$(HERE)/pkg.$*/install" --with-compiler="$(TEST_HC)" --with-hc-pkg="$(GHC_PKG)" --package-db=pkg.$*/local.package.conf
	pkg.$*/setup build     --distdir pkg.$*/dist -v0
	pkg.$*/setup install   --distdir pkg.$*/dist -v0
