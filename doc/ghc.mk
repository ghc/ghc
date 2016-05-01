# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------

ifeq "$(BUILD_SPHINX_HTML)" "YES"
INSTALL_HTML_DOC_DIRS += utils/haddock/doc/haddock

html : html_utils/haddock/doc
endif

html_utils/haddock/doc :
	make -C utils/haddock/doc html SPHINX_BUILD=$(SPHINXBUILD)
	cp -R utils/haddock/doc/.build-html utils/haddock/doc/haddock

