#	General makefile for Latex stuff

dvi: sm.dvi rp.dvi ldv.dvi
ps: sm.ps rp.ps ldv.ps
html:

######## General rules
.SUFFIXES:
.PRECIOUS: %.tex %.ps %.bbl

#%.dvi: %.tex $(addsuffix .tex, $(basename $(wildcard *.verb *.fig))) $(wildcard *.bib)
%.dvi: %.tex $(addsuffix .tex, $(basename $(wildcard *.verb))) $(wildcard *.bib)
	latex $<
	@if grep -s "\citation" $*.aux; then bibtex $*; fi
	latex $<
	latex $<

%.ps: %.dvi
	dvips -f < $< > $@

clean:
	$(RM) *.aux *.log

distclean: clean
	$(RM) *.dvi *.ps *.bbl *.blg *.gz

maintainer-clean: distclean

all:

boot:

# End of file
