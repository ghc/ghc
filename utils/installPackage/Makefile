
TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/cabal.mk

# If the bootstrapping compiler already has a newer version of Cabal
# than the in-tree Cabal, then we need to forcibly use the in-tree one.
# Otherwise we use one Cabal for some things and the other Cabal for
# other things, and Cabal (rightly) complains about the version
# mismatch.
EXTRA_INPLACE_CONFIGURE_FLAGS += $(CABAL_CONSTRAINT)
EXTRA_STAGE2_CONFIGURE_FLAGS  += $(CABAL_CONSTRAINT)

