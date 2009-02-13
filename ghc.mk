
utils/haddock_USES_CABAL = YES
utils/haddock_PACKAGE = haddock
utils/haddock_CONFIGURE_OPTS = --flag in-ghc-tree
utils/haddock_dist_PROG = haddock
# XXX This is a bit of a hack. We mkdepend with stage1 as if .depend
# depends on the stage2 compiler then make goes wrong: haddock's
# .depend gets included, which means that make won't reload until
# it's built, but we can't build it without the stage2 compiler. We
# therefore build the stage2 compiler before its .depend file is
# available, and so compilation fails.
utils/haddock_dist_HC_DEP = $(GHC_STAGE1)
# XXX This is a temporary hack:
utils/haddock_HC_OPTS += -Wwarn

$(eval $(call build-prog,utils/haddock,dist,2))

utils/haddock_dist_MODULES += Paths_haddock

