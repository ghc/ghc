TOP = ../..
include $(TOP)/mk/boilerplate.mk

SGML_DOC = users_guide
INSTALL_SGML_DOC = users_guide

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
