BuildFlavour = quick-cross-ncg
ifneq "$(BuildFlavour)" ""
include mk/flavours/$(BuildFlavour).mk
endif
Stage1Only = YES
HADDOCK_DOCS = NO
BUILD_SPHINX_HTML = NO
BUILD_SPHINX_PDF = NO
GhcLibHcOpts += -fPIC -keep-s-file
GhcRtsHcOpts += -fPIC