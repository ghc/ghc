TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

CABAL_SETUP = $(FPTOOLS_TOP_ABS)/libraries/Cabal/cabal-setup/cabal-setup

CONFIG_ARGS = --with-compiler=$(TEST_HC) --with-hc-pkg=$(GHC_PKG_DIR_ABS)/$(GHC_PKG_INPLACE_PGM)

# We have 3 packages, P-1.0, P-2.0, and Q.  Q depends on P-1.0.
# We have an executable R, that depends on P-2.0 and Q, so the resulting
# executable will link to both P-1.0 and P-2.0.

cabal02 : 
	(cd P1 && $(CABAL_SETUP) configure $(CONFIG_ARGS))
	(cd P1 && $(CABAL_SETUP) build)
	(cd P1 && $(CABAL_SETUP) register --inplace --user)
	(cd P2 && $(CABAL_SETUP) configure $(CONFIG_ARGS))
	(cd P2 && $(CABAL_SETUP) build)
	(cd P2 && $(CABAL_SETUP) register --inplace --user)
	(cd Q  && $(CABAL_SETUP) configure $(CONFIG_ARGS))
	(cd Q  && $(CABAL_SETUP) build)
	(cd Q  && $(CABAL_SETUP) register --inplace --user)
	(cd R  && $(CABAL_SETUP) configure $(CONFIG_ARGS))
	(cd R  && $(CABAL_SETUP) build)
	$(MAKE) clean

clean ::
	(cd P1 && $(CABAL_SETUP) unregister --user)
	(cd P2 && $(CABAL_SETUP) unregister --user)
	(cd Q  && $(CABAL_SETUP) unregister --user)
	(cd P1 && $(CABAL_SETUP) clean)
	(cd P2 && $(CABAL_SETUP) clean)
	(cd Q  && $(CABAL_SETUP) clean)
