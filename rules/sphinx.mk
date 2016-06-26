# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------


# Build Sphinx documentation

# We are careful not to use the same directory the doctree files for the
# various Sphinx targets as make may run them in parallel (see #10950).

define sphinx
$(call trace, sphinx($1,$2))
$(call profStart, sphinx($1,$2))
# $1 = dir
# $2 = docname

$(eval $(call clean-target,$1,sphinx,$1/.doctrees-html/ $1/.doctrees-pdf/ $1/build-html/ $1/build-pdf/ $1/$2.pdf))

# empty "all_$1" target just in case we're not building docs at all
$(call all-target,$1,)

.PHONY: html_$1
ifeq "$$(phase)" "final"
ifeq "$$(BUILD_SPHINX_HTML)" "YES"
$(call all-target,$1,html_$1)
INSTALL_HTML_DOC_DIRS += $1/build-html/$2
endif
endif

html_$1 : $1/build-html/$2/index.html
html : html_$1

ifneq "$$(BINDIST)" "YES"
$1/build-html/$2/index.html: $1/conf.py $$($1_RST_SOURCES)
	$(SPHINXBUILD) -b html -d $1/.doctrees-html $(SPHINXOPTS) $1 $1/build-html/$2
endif


.PHONY: pdf_$1
ifeq "$$(phase)" "final"
ifeq "$$(BUILD_SPHINX_PDF)" "YES"
$(call all-target,$1,pdf_$1)
INSTALL_DOCS += $1/$2.pdf
endif
endif

pdf_$1 : $1/$2.pdf
pdf : pdf_$1

ifneq "$$(BINDIST)" "YES"
$1/$2.pdf: $1/conf.py $$($1_RST_SOURCES)
	$(SPHINXBUILD) -b latex -d $1/.doctrees-pdf $(SPHINXOPTS) $1 $1/build-pdf/$2
	cd $1/build-pdf/$2 ; xelatex -halt-on-error $2.tex
	cd $1/build-pdf/$2 ; xelatex -halt-on-error $2.tex
	cd $1/build-pdf/$2 ; xelatex -halt-on-error $2.tex
	cd $1/build-pdf/$2 ; makeindex $2.idx
	cd $1/build-pdf/$2 ; xelatex -halt-on-error $2.tex
	cd $1/build-pdf/$2 ; xelatex -halt-on-error $2.tex
	cp $1/build-pdf/$2/$2.pdf $1/$2.pdf
endif


$(call profEnd, sphinx($1,$2))
endef
