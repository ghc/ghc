all: backpack-impl.pdf backpack-manual.pdf ubackpack.pdf backpack-shaping.pdf algorithm.pdf

ubackpack.pdf: ubackpack.tex
	latexmk -pdf -latexoption=-halt-on-error -latexoption=-file-line-error -latexoption=-synctex=1 ubackpack.tex || ! rm -f $@

backpack-impl.pdf: backpack-impl.tex
	latexmk -pdf -latexoption=-halt-on-error -latexoption=-file-line-error -latexoption=-synctex=1 backpack-impl.tex || ! rm -f $@

backpack-manual.pdf: backpack-manual.tex
	latexmk -pdf -latexoption=-halt-on-error -latexoption=-file-line-error -latexoption=-synctex=1 backpack-manual.tex || ! rm -f $@

backpack-shaping.pdf: backpack-shaping.tex
	latexmk -pdf -latexoption=-halt-on-error -latexoption=-file-line-error -latexoption=-synctex=1 backpack-shaping.tex || ! rm -f $@

algorithm.pdf: algorithm.tex
	latexmk -pdf -latexoption=-halt-on-error -latexoption=-file-line-error -latexoption=-synctex=1 algorithm.tex || ! rm -f $@
