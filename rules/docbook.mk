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


# Build docbook docs

define docbook
$(call trace, docbook($1,$2))
$(call profStart, docbook($1,$2))
# $1 = dir
# $2 = docname

$(call clean-target,$1,docbook,$1/$2 $1/$2.pdf $1/$2.ps)

# empty "all_$1" target just in case we're not building docs at all
$(call all-target,$1,)

.PHONY: html_$1

ifeq "$$(phase)" "final"
ifeq "$$(BUILD_DOCBOOK_HTML)" "YES"
$(call all-target,$1,html_$1)
INSTALL_HTML_DOC_DIRS += $1/$2
endif
endif

html_$1 : $1/$2/index.html

ifneq "$$(BINDIST)" "YES"
$1/$2/index.html: $$($1_DOCBOOK_SOURCES)
	$$(call removeTrees,$$(dir $$@))
	"$$(XSLTPROC)" --stringparam base.dir $$(dir $$@) \
	               --stringparam use.id.as.filename 1 \
	               --stringparam html.stylesheet fptools.css \
	               --nonet \
	               $$(XSLTPROC_LABEL_OPTS) $$(XSLTPROC_OPTS) \
	               $$(XSLTPROC_HTML_STYLESHEET) \
	               $1/$2.xml
	cp mk/fptools.css $$(dir $$@)
endif


.PHONY: ps_$1
ifeq "$$(phase)" "final"
ifeq "$$(BUILD_DOCBOOK_PS)" "YES"
$(call all-target,$1,ps_$1)
INSTALL_DOCS += $1/$2.ps
endif
endif

ps_$1 : $1/$2.ps

ifneq "$$(BINDIST)" "YES"
$1/$2.ps: $$($1_DOCBOOK_SOURCES)
	"$$(DBLATEX)" $$(DBLATEX_OPTS) $1/$2.xml --ps -o $$@
	[ -f $$@ ]
endif

ifeq "$$(phase)" "final"
ifeq "$$(BUILD_DOCBOOK_PDF)" "YES"
$(call all-target,$1,pdf_$1)
INSTALL_DOCS += $1/$2.pdf
endif
endif

.PHONY: pdf_$1
pdf_$1 : $1/$2.pdf

ifneq "$$(BINDIST)" "YES"
$1/$2.pdf: $$($1_DOCBOOK_SOURCES)
	"$$(DBLATEX)" $$(DBLATEX_OPTS) $1/$2.xml --pdf -o $$@
	[ -f $$@ ]
endif

$(call profEnd, docbook($1,$2))
endef

