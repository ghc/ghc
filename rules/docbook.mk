
# Build docbook docs

define docbook
# $1 = dir
# $2 = docname

$(call clean-target,$1,docbook,$1/$2)

ifneq "$$(XSLTPROC)" ""
$(call all-target,$1,$1/$2/index.html)

$1/$2/index.html: $$($1_DOCBOOK_SOURCES)
	$$(RM) -r $$(dir $$@)
	$$(XSLTPROC) --stringparam base.dir $$(dir $$@) \
	             --stringparam use.id.as.filename 1 \
	             --stringparam html.stylesheet fptools.css \
	             $$(XSLTPROC_LABEL_OPTS) $$(XSLTPROC_OPTS) \
	             $$(DIR_DOCBOOK_XSL)/html/chunk.xsl $1/$2.xml
	cp mk/fptools.css $$(dir $$@)
endif

endef

