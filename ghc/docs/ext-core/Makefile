#	General makefile for Latex stuff

dvi: core.dvi
ps: core.ps
html:

core.dvi:	core.tex prims.tex
		latex core.tex
		latex core.tex

../../compiler/prelude/primops.txt: ../../compiler/prelude/primops.txt.pp
		(cd ../../compiler/prelude; gcc -E -I../../includes -traditional -x c primops.txt.pp | /bin/sed -e '/^#/d' > primops.txt)

prims.tex:	../../compiler/prelude/primops.txt
		../../utils/genprimopcode/genprimopcode --make-latex-doc < ../../compiler/prelude/primops.txt > prims.tex


######## General rules
.SUFFIXES:
.PRECIOUS: %.tex %.ps %.bbl


%.ps: %.dvi
	dvips -f < $< > $@

clean:
	$(RM) *.aux *.log

distclean: clean
	$(RM) prims.tex *.dvi *.ps *.bbl *.blg *.gz

maintainer-clean: distclean

all:

boot:

# End of file
