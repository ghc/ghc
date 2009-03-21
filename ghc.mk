
utils/haddock_USES_CABAL = YES
utils/haddock_PACKAGE = haddock
utils/haddock_CONFIGURE_OPTS = --flag in-ghc-tree
# XXX This is a temporary hack:
utils/haddock_HC_OPTS += -Wwarn
utils/haddock_dist_SHELL_WRAPPER = YES
utils/haddock_dist_PROG = haddock
# XXX Is this a hack? Should it be needed? Done another way?
utils/haddock_dist_DATADIR = lib

$(INPLACE_BIN)/$(utils/haddock_dist_PROG): inplace/lib/html

inplace/lib/html:
	$(RM) -rf $@
	cp -a utils/haddock/html $@

INSTALL_LIBEXECS += utils/haddock/dist/build/$(utils/haddock_dist_PROG)

$(eval $(call build-prog,utils/haddock,dist,2))

utils/haddock_dist_MODULES += Paths_haddock
