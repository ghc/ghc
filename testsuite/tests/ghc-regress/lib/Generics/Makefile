CWD=${shell pwd}
TOP=${CWD}/../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Reference to stage2 compiler supporting interactive use
stage2=${TOP}/../ghc/compiler/stage2/ghc-inplace

# Run the infamous paradise benchmark
tldi03:
	gmake TEST_HC=${stage2} WAY=ghci TESTS=paradise

# A tweaked main target to use a stage 2 compiler
stage2:
	gmake TEST_HC=${stage2}

# Just do the ghci way for testing during development
ghci:
	gmake TEST_HC=${stage2} WAY=ghci

# Just do normal compilation for testing during development
normal:
	gmake WAY=normal

# Should probably be reused from somewhere else
clean:
	find . -depth -name "core"       -exec rm -rf \{\} \;
	find . -depth -name "Main"       -exec rm -rf \{\} \;
	find . -depth -name "*~"         -exec rm -rf \{\} \;
	find . -depth -name "*.o"        -exec rm -rf \{\} \;
	find . -depth -name "*.hi"       -exec rm -rf \{\} \;
	find . -depth -name "*.prof"     -exec rm -rf \{\} \;
	find . -depth -name "*.script"   -exec rm -rf \{\} \;
	find . -depth -name "*.comp.*"   -exec rm -rf \{\} \;
	find . -depth -name "*.interp.*" -exec rm -rf \{\} \;
	find . -depth -name "*.run.*"    -exec rm -rf \{\} \;
	rm -f foldTree
	rm -f freeNames/freeNames
	rm -f geq/geq
	rm -f gmapQ-assoc
	rm -f gread
	rm -f gshow/gshow
	rm -f gsize
	rm -f gzip/gzip
	rm -f local-quantors
	rm -f newtype
	rm -f paradise/paradise
	rm -f shortcut
	rm -f strings/strings
	rm -f where
