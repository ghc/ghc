TOP = ../../..
include $(TOP)/ghc/mk/ghc.mk

state-interface.dvi : state-interface.tex
	$(LATEX) state-interface.tex

state-interface.tex : state-interface.verb
	@$(RM) state-interface.tex
	expand state-interface.verb | verbatim > state-interface.tex
