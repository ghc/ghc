TOP = ../..
include $(TOP)/mk/boilerplate.mk

ifeq "$(BIN_DIST)" "1"
install :: html dvi info
endif

# Split up the pages on a per-sect2 basis. Big HTML docs
# are a pain to flick through and use.
SRC_SGML2HTML_OPTS += -s 1

SGML_DOC = users_guide

# Generating index is semi-automatic, you need to:
# 
#    make users_guide.dvi ; make index ; make users_guide.dvi
# 
# To work, it depends on you tweaking
#
#      lib/sgml-tools/dist/sgmltool/latex2e/mapping
#
# to instead of saying
#
# </article>      +       "\\end{document}"       +
#
# say 
#
# </article>      +       "\\InputIfFileExists{index}{}{}"
#                         "\\end{document}"       +
# 
# That partially works around the problem, if nothing else.
# 
index : users_guide.dvi
	makeindex users_guide
	@$(RM) $@
	$(CP) users_guide.ind $@
	$(RM) users_guide.dvi


include $(TOP)/mk/target.mk
