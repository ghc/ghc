TOP = ../..
include $(TOP)/mk/boilerplate.mk

ifeq "$(LATEX_DOCS)" "YES"
all: pdf

install-docs:
	-mkdir      $(docdir)/ext-core
	cp core.pdf $(docdir)/ext-core/
else
all:
install-docs:
endif

#	General makefile for Latex stuff

LATEX=latex \\nonstopmode \\input
PDFLATEX=pdflatex
BIBTEX=bibtex

dvi: core.dvi
ps: core.ps core.ps.gz
pdf: core.pdf

core.pdf: core.tex
	$(PDFLATEX) core.tex
	$(BIBTEX) core
	$(PDFLATEX) core.tex
	$(PDFLATEX) core.tex

######## General rules
.SUFFIXES:
.PRECIOUS: %.tex %.ps %.ps.gz %.pdf %.bbl


%.gz: %
	gzip < $< > $@

%.ps: %.dvi
	dvips -f < $< > $@

%.pdf: %.ps
	ps2pdf $< $@

clean:
	$(RM) *.aux *.log

distclean: clean
	$(RM) prims.tex *.dvi *.ps *.pdf *.bbl *.blg *.gz

maintainer-clean: distclean

include $(TOP)/mk/bindist.mk

# dummy targets
boot:
install:
html:
chm:
HxS:

# End of file
