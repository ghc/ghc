all: backpack-impl.pdf backpack-manual.pdf

backpack-impl.pdf: backpack-impl.tex
	latexmk -pdf -latexoption=-halt-on-error -latexoption=-file-line-error -latexoption=-synctex=1 backpack-impl.tex && touch paper.dvi || ! rm -f $@

backpack-manual.pdf: backpack-manual.tex
	latexmk -pdf -latexoption=-halt-on-error -latexoption=-file-line-error -latexoption=-synctex=1 backpack-manual.tex && touch paper.dvi || ! rm -f $@
