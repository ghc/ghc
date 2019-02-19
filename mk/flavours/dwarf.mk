# Build flavour which produces a compiler, RTS, and core libraries with DWARF
# debug information. For best results run ./configure with
# --enable-dwarf-unwind.

SRC_HC_OPTS        = -O -H64m
GhcStage1HcOpts    = -O2
GhcStage2HcOpts    = -O2 -g3
GhcRtsHcOpts       = -O2 -g3
GhcLibHcOpts       = -O2 -g3
BUILD_PROF_LIBS    = YES
#SplitObjs
#HADDOCK_DOCS
#BUILD_SPHINX_HTML
#BUILD_SPHINX_PDF
