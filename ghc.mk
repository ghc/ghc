
utils/haddock_USES_CABAL = YES
utils/haddock_PACKAGE = haddock
utils/haddock_CONFIGURE_OPTS = --flag in-ghc-tree
# XXX This is a temporary hack:
utils/haddock_HC_OPTS += -Wwarn

ifeq "$(Windows)" "YES"
utils/haddock_dist_PROG = haddock
else
utils/haddock_dist_PROG = haddock-real

$(INPLACE_BIN)/haddock: $(INPLACE_BIN)/haddock-real
	$(RM) -f $@
	echo '#!$(SHELL)' >> $@
	echo 'executablename=$(FPTOOLS_TOP_ABS)/$<' >> $@
	echo 'datadir=$(FPTOOLS_TOP_ABS)/inplace/lib' >> $@
	cat utils/haddock/haddock.wrapper >> $@
	$(EXECUTABLE_FILE) $@
endif

$(eval $(call build-prog,utils/haddock,dist,2))

utils/haddock_dist_MODULES += Paths_haddock

