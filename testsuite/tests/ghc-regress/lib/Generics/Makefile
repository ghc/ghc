TOP=../../../..
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

# Just do the normal ghc way for testing during development
normal:
	gmake WAY=normal

# Just do the ghci way for testing during development
ghci:
	gmake TEST_HC=${stage2} WAY=ghci

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
	rm -f bits/bits
	rm -f ext1/ext1
	rm -f datatype
	rm -f genUpTo/genUpTo
	rm -f foldTree
	rm -f geq/geq
	rm -f gmapQ-assoc
	rm -f gread
	rm -f gread2
	rm -f gshow2
	rm -f gshow/gshow
	rm -f gzip/gzip
	rm -f hopat
	rm -f local-quantors
	rm -f labels
	rm -f mega-model/mega-model
	rm -f nested-datatypes
	rm -f newtype
	rm -f perm
	rm -f hlist
	rm -f typecase1
	rm -f typecase2
	rm -f paradise/paradise
	rm -f reify/reify
	rm -f strings/strings
	rm -f tree/tree
	rm -f twin
	rm -f where
	rm -f xmlish/xmlish
