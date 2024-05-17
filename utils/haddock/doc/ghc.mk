# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------

ifeq "$(BUILD_SPHINX_HTML)" "YES"
html : html_utils/haddock/doc

$(eval $(call clean-target,utils/haddock/doc,sphinx,utils/haddock/doc/haddock utils/haddock/doc/.build-html utils/haddock/doc/.build-pdf))
$(eval $(call all-target,utils/haddock/doc,html_utils/haddock/doc))
INSTALL_HTML_DOC_DIRS += utils/haddock/doc/haddock
endif

html_utils/haddock/doc :
	$(MAKE) -C utils/haddock/doc html SPHINX_BUILD=$(SPHINXBUILD)
	cp -R utils/haddock/doc/.build-html utils/haddock/doc/haddock

