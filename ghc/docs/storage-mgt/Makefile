#	General makefile for Latex stuff

ps: sm.ps rp.ps ldv.ps


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
	rm -f *.aux *.log

nuke: clean
	rm -f *.dvi *.ps *.bbl *.blg

# End of file
