#	General makefile for Latex stuff

ps: core.ps

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
	rm -f *.aux *.log

nuke: clean
	rm -f *.dvi *.ps *.bbl *.blg

# End of file
