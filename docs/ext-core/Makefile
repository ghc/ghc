TOP = ../..
include $(TOP)/mk/boilerplate.mk

ifeq "$(LATEX_DOCS)" "YES"
all: ps

install-docs:
	-mkdir        $(docdir)/ext-core
	cp core.ps.gz $(docdir)/ext-core/
else
all:
endif

#	General makefile for Latex stuff

LATEX=latex \\nonstopmode \\input
BIBTEX=bibtex

dvi: core.dvi
ps: core.ps core.ps.gz

core.dvi:	core.tex
		-$(LATEX) core.tex
	        $(BIBTEX) core
		-$(LATEX) core.tex
		-$(LATEX) core.tex

######## General rules
.SUFFIXES:
.PRECIOUS: %.tex %.ps %.ps.gz %.bbl


%.gz: %
	gzip < $< > $@

%.ps: %.dvi
	dvips -f < $< > $@

clean:
	$(RM) *.aux *.log

distclean: clean
	$(RM) prims.tex *.dvi *.ps *.bbl *.blg *.gz

maintainer-clean: distclean

BINDIST_EXTRAS += core.ps.gz
include $(TOP)/mk/bindist.mk

# dummy targets
boot:
install:
html:
chm:
HxS:

# End of file
