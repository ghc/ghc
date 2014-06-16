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

docs/users_guide_GENERATED_DOCBOOK_SOURCES := \
	docs/users_guide/users_guide.xml                \
	docs/users_guide/what_glasgow_exts_does.gen.xml

# sort remove duplicates
docs/users_guide_DOCBOOK_SOURCES :=                           \
    $(sort $(docs/users_guide_GENERATED_DOCBOOK_SOURCES)      \
           $(wildcard docs/users_guide/*.xml)                 \
           $(basename $(wildcard docs/users_guide/*.xml.in)))

$(docs/users_guide_GENERATED_DOCBOOK_SOURCES): %.xml: $(mkUserGuidePart_INPLACE)
	$(mkUserGuidePart_INPLACE) $@

$(eval $(call docbook,docs/users_guide,users_guide))

$(eval $(call clean-target,docs/users_guide,gen,$(docs/users_guide_GENERATED_DOCBOOK_SOURCES)))

html_docs/users_guide : docs/users_guide/users_guide/prof_scc.png

docs/users_guide/users_guide/prof_scc.png : \
		docs/users_guide/prof_scc.png \
		docs/users_guide/users_guide/index.html
	$(CP) $< $@
# dep. on d/u/u/index.html is to make sure that the d/u/u dir is created first

