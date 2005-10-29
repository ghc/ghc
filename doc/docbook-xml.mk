#-----------------------------------------------------------------------------
# DocBook XML

.PHONY: html html-no-chunks chm HxS fo dvi ps pdf

ifneq "$(XML_DOC)" ""

all :: html

# multi-file XML document: main document name is specified in $(XML_DOC),
# sub-documents (.xml files) listed in $(XML_SRCS).

ifeq "$(XML_SRCS)" ""
XML_SRCS = $(wildcard *.xml)
endif

XML_HTML           = $(addsuffix /index.html,$(basename $(XML_DOC)))
XML_HTML_NO_CHUNKS = $(addsuffix .html,$(XML_DOC))
XML_CHM            = $(addsuffix .chm,$(XML_DOC))
XML_HxS            = $(addsuffix .HxS,$(XML_DOC))
XML_FO             = $(addsuffix .fo,$(XML_DOC))
XML_DVI            = $(addsuffix .dvi,$(XML_DOC))
XML_PS             = $(addsuffix .ps,$(XML_DOC))
XML_PDF            = $(addsuffix .pdf,$(XML_DOC))

$(XML_HTML) $(XML_NO_CHUNKS_HTML) $(XML_FO) $(XML_DVI) $(XML_PS) $(XML_PDF) :: $(XML_SRCS)

html           :: $(XML_HTML)
html-no-chunks :: $(XML_HTML_NO_CHUNKS)
chm            :: $(XML_CHM)
HxS            :: $(XML_HxS)
fo             :: $(XML_FO)
dvi            :: $(XML_DVI)
ps             :: $(XML_PS)
pdf            :: $(XML_PDF)

CLEAN_FILES += $(XML_HTML_NO_CHUNKS) $(XML_FO) $(XML_DVI) $(XML_PS) $(XML_PDF)

FPTOOLS_CSS     = fptools.css

clean ::
	$(RM) -rf $(XML_DOC).out $(basename $(XML_DOC)) $(basename $(XML_DOC))-htmlhelp

validate ::
	$(XMLLINT) --valid --noout $(XMLLINT_OPTS) $(XML_DOC).xml
endif

#-----------------------------------------------------------------------------
# DocBook XML suffix rules
#

%.html : %.xml
	$(XSLTPROC) --output $@ \
		    --stringparam html.stylesheet $(FPTOOLS_CSS) \
		    $(XSLTPROC_LABEL_OPTS) $(XSLTPROC_OPTS) \
		    $(DIR_DOCBOOK_XSL)/html/docbook.xsl $<

%/index.html : %.xml
	$(RM) -rf $(dir $@)
	$(XSLTPROC) --stringparam base.dir $(dir $@) \
		    --stringparam use.id.as.filename 1 \
		    --stringparam html.stylesheet $(FPTOOLS_CSS) \
		    $(XSLTPROC_LABEL_OPTS) $(XSLTPROC_OPTS) \
		    $(DIR_DOCBOOK_XSL)/html/chunk.xsl $<
	cp $(FPTOOLS_CSS) $(dir $@)

# Note: Numeric labeling seems to be uncommon for HTML Help
%-htmlhelp/index.html : %.xml
	$(RM) -rf $(dir $@)
	$(XSLTPROC) --stringparam base.dir $(dir $@) \
		    --stringparam manifest.in.base.dir 1 \
		    --stringparam htmlhelp.chm "..\\"$(basename $<).chm \
		    $(XSLTPROC_OPTS) \
		    $(DIR_DOCBOOK_XSL)/htmlhelp/htmlhelp.xsl $<

%-htmlhelp2/collection.HxC : %.xml
	$(RM) -rf $(dir $@)
	$(XSLTPROC) --stringparam base.dir $(dir $@) \
		    --stringparam use.id.as.filename 1 \
		    --stringparam manifest.in.base.dir 1 \
		    $(XSLTPROC_OPTS) \
		    $(DIR_DOCBOOK_XSL)/htmlhelp2/htmlhelp2.xsl $<

# TODO: Detect hhc & Hxcomp via autoconf
#
# Two obstacles here:
#
# * The reason for the strange "if" below is that hhc returns 0 on error and 1
#   on success, the opposite of what shells and make expect.
#
# * There seems to be some trouble with DocBook indices, but the *.chm looks OK,
#   anyway, therefore we pacify make by "|| true". Ugly...
#
%.chm : %-htmlhelp/index.html
	( cd $(dir $<) && if hhc htmlhelp.hhp ; then false ; else true ; fi ) || true

%.HxS : %-htmlhelp2/collection.HxC
	( cd $(dir $<) && if Hxcomp -p collection.HxC -o ../$@ ; then false ; else true ; fi )

%.fo : %.xml
	$(XSLTPROC) --output $@ \
		    --stringparam draft.mode no \
		    $(XSLTPROC_LABEL_OPTS) $(XSLTPROC_OPTS) \
		    $(DIR_DOCBOOK_XSL)/fo/docbook.xsl $<

ifeq "$(FOP)" ""
ifneq "$(PDFXMLTEX)" ""
%.pdf : %.fo
	$(PDFXMLTEX) $<
	if grep "LaTeX Warning: Label(s) may have changed.Rerun to get cross-references right." $(basename $@).log > /dev/null ; then \
	  $(PDFXMLTEX) $< ; \
	  $(PDFXMLTEX) $< ; \
	fi
endif
else
%.ps : %.fo
	$(FOP) $(FOP_OPTS) -fo $< -ps $@

%.pdf : %.fo
	$(FOP) $(FOP_OPTS) -fo $< -pdf $@
endif

ifneq "$(XMLTEX)" ""
%.dvi : %.fo
	$(XMLTEX) $<
	if grep "LaTeX Warning: Label(s) may have changed.Rerun to get cross-references right." $(basename $@).log > /dev/null ; then \
	  $(XMLTEX) $< ; \
	  $(XMLTEX) $< ; \
	fi
endif
