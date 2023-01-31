TOP=../../..
RUN=-DRUN1
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean.%:
	rm -rf pkg.$*

HERE := $(abspath .)
$(eval $(call canonicalise,HERE))

package.%:
	$(MAKE) -s --no-print-directory clean.$*
	mkdir pkg.$*
	"$(TEST_HC)" -outputdir pkg.$* --make -v0 -o pkg.$*/setup Setup.hs

	"$(GHC_PKG)" init pkg.$*/local.package.conf

  # The bogus extra-lib-dirs ensures the package is registered with multiple
  # dynamic-library-directories which tests that the fix for #15475 works
	pkg.$*/setup configure --distdir pkg.$*/dist -v0 $(CABAL_PLUGIN_BUILD) --ghc-option="$(RUN)" --prefix="$(HERE)/pkg.$*/install" --with-compiler="$(TEST_HC)" --with-hc-pkg="$(GHC_PKG)" --extra-lib-dirs="$(HERE)" --package-db=pkg.$*/local.package.conf $(if $(findstring YES,$(HAVE_PROFILING)), --enable-library-profiling)
  # -dno-debug-output: Suppress warnings ("Simplifier bailing out ...") while
  # building the plugin package. It's just too tedious to figure out what goes
  # wrong and likely shows up in other tests as well.
	pkg.$*/setup build     --distdir pkg.$*/dist -v0 --ghc-option="-dno-debug-output"
	pkg.$*/setup install   --distdir pkg.$*/dist -v0
