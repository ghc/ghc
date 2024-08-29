SPHINX_BUILD ?= sphinx-build

all : html pdf

.PHONY : html
.PHONY : pdf

html :
	$(SPHINX_BUILD) -b html . .build-html

pdf :
	$(SPHINX_BUILD) -b latex . .build-latex
	cd .build-latex && make
