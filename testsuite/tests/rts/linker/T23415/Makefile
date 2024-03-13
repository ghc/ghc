TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

REUSE_DB=db
REUSE_CONF=reuse.conf
REUSE_PKG=pkg-reuse

.PHONY: T23415a
T23415a:
	mkdir -p "$(REUSE_PKG)"/{lib,hi}
	mkdir -p src
	mkdir -p "$(REUSE_DB)"
	mv Reuse.hs ReusePlugin.hs src/
	"$(TEST_HC)" $(filter-out -rtsopts, $(TEST_HC_OPTS)) \
		-dynamic-too \
		-hidir "$(REUSE_PKG)/hi/" \
		-O0 \
		-this-unit-id reuse-1.0 \
		-package ghc \
		-c src/*
	"$(TEST_HC)" $(filter-out -rtsopts, $(TEST_HC_OPTS)) \
		-dynamic -shared -fPIC \
		-hidir "$(REUSE_PKG)/hi/" \
		-this-unit-id reuse-1.0 \
		-package ghc \
		-O0 \
		-o "$(REUSE_PKG)/lib/libHSreuse-1.0-ghc$(shell "$(TEST_HC)" --numeric-version)$(dllext)" src/*.dyn_o
	"$(GHC_PKG)" --no-user-package-db --package-db="$(REUSE_DB)" recache
	"$(GHC_PKG)" --no-user-package-db --package-db="$(REUSE_DB)" register "$(REUSE_CONF)"
	"$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) \
		-package-db $(REUSE_DB) \
		-v0 +RTS -Dl -RTS \
		-fplugin=ReusePlugin \
		-ghci-script Reuse.script
