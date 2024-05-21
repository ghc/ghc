TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: T12935
T12935:
	$(RM) -rf objs1 objs2
	cabal get Cabal-3.12.0.0
	# Ignore that redirecting ofiles elsewhere will cause compilation to fail
	# because cabal invokes `ar` on the expected object files directly...
	cabal build -w '$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --ghc-options=-odir=out1 Cabal > /dev/null 2>&1 || true
	cabal build -w '$(TEST_HC)' --ghc-options='$(TEST_HC_OPTS)' --ghc-options=-odir=out2 Cabal > /dev/null 2>&1 || true
	./check.sh $(HostOS)


