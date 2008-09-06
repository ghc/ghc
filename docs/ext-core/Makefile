TOP = ../..
include $(TOP)/mk/boilerplate.mk

all: ps

install-docs:
	-mkdir        $(docdir)/ext-core
	cp core.ps.gz $(docdir)/ext-core/

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

include $(TOP)/mk/bindist.mk

# dummy targets
boot:
install:
html:
chm:
HxS:

# End of file
