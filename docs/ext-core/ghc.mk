
ifeq "$(LATEX_DOCS)" "YES"
$(eval $(call all-target,docs/ext-core,docs/ext-core/core.pdf))

INSTALL_DOCS += docs/ext-core/core.pdf
endif

ifneq "$(BINDIST)" "YES"
docs/ext-core/core.pdf: docs/ext-core/core.tex
	cd docs/ext-core && $(PDFLATEX) core.tex
	cd docs/ext-core && $(BIBTEX) core
	cd docs/ext-core && $(PDFLATEX) core.tex
	cd docs/ext-core && $(PDFLATEX) core.tex
endif

