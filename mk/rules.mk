#-----------------------------------------------------------------------------
# $Id: rules.mk,v 1.2 1996/11/21 16:50:38 simonm Exp $

# This file defines the default suffix rules.  It replaces suffixes.jm in the
# jmake system.

.SUFFIXES: .xdvi .ps .dvi .tex .fig .tib .verb .itex .itxi .ihtml .lit \
	.p_o .o .s .hi .hc .lhc .lhs .hs .prl .lprl .sh .lsh \
	.c .lc .h .lh .flex .lflex .y

ifndef SuffixRules_WantStdOnes
SuffixRules_WantStdOnes = YES /* but you can turn it off */
endif

#-----------------------------------------------------------------------------
# Doc processing suffix rules

ifdef DocProcessingSuffixRules

.tex.dvi:
	@$(RM) $@
	$(LTX) $<

.verb.tex:
	@$(RM) $*.tex
	expand $*.verb | $(VERBATIM) > $*.tex

.tib.tex:
	@$(RM) $*.tex $*.verb-t.tex
	$(TIB) $*.tib
	expand $*.tib-t.tex | $(VERBATIM) > $*.tex
	@$(RM) $*.tib-t.tex

.fig.ps:
	@$(RM) $@
	fig2dev -L ps $< $@

.fig.tex:
	@$(RM) $@
	fig2dev -L latex $< $@

endif

#-----------------------------------------------------------------------------
# Literate suffix rules

# ToDo: somehow macroize this lot.

ifdef LiterateSuffixRules

.lit:
	@$(RM) $@
	$(LIT2PGM) $(LIT2PGMFLAGS) -o $@ $<
	@chmod 444 $@

.lit.itxi:
	@$(RM) $@
	$(LIT2TEXI) -c $(LIT2TEXIFLAGS) -o $@ $<
	@chmod 444 $@

.lit.ihtml:
	@$(RM) $@
	$(LIT2HTML) -c $(LIT2HTMLFLAGS) -o $@ $<
	@chmod 444 $@

.lit.itex:
	@$(RM) $@
	$(LIT2LATEX) -c $(LIT2LATEXFLAGS) -o $@ $<
	@chmod 444 $@

.lhs.hs:
	@$(RM) $@
	$(LIT2PGM) $(LIT2PGMFLAGS) -o $@ $<
	@chmod 444 $@

.lhs.itxi:
	@$(RM) $@
	$(LIT2TEXI) -c $(LIT2TEXIFLAGS) -o $@ $<
	@chmod 444 $@

.lhs.ihtml:
	@$(RM) $@
	$(LIT2HTML) -c $(LIT2HTMLFLAGS) -o $@ $<
	@chmod 444 $@

.lhs.itex:
	@$(RM) $@
	$(LIT2LATEX) -c $(LIT2LATEXFLAGS) -o $@ $<
	@chmod 444 $@

.lhc.hc:
	@$(RM) $@
	$(LIT2PGM) $(LIT2PGMFLAGS) -o $@ $<
	@chmod 444 $@

.lhc.itxi:
	@$(RM) $@
	$(LIT2TEXI) -c $(LIT2TEXIFLAGS) -o $@ $<
	@chmod 444 $@

.lhc.ihtml:
	@$(RM) $@
	$(LIT2HTML) -c $(LIT2HTMLFLAGS) -o $@ $<
	@chmod 444 $@

.lhc.itex:
	@$(RM) $@
	$(LIT2LATEX) -c $(LIT2LATEXFLAGS) -o $@ $<
	@chmod 444 $@

.lprl.prl:
	@$(RM) $@
	$(LIT2PGM) $(LIT2PGMFLAGS) -o $@ $<
	@chmod 444 $@

.lprl.itxi:
	@$(RM) $@
	$(LIT2TEXI) -c $(LIT2TEXIFLAGS) -o $@ $<
	@chmod 444 $@

.lprl.ihtml:
	@$(RM) $@
	$(LIT2HTML) -c $(LIT2HTMLFLAGS) -o $@ $<
	@chmod 444 $@

.lprl.itex:
	@$(RM) $@
	$(LIT2LATEX) -c $(LIT2LATEXFLAGS) -o $@ $<
	@chmod 444 $@

.lsh.sh:
	@$(RM) $@
	$(LIT2PGM) $(LIT2PGMFLAGS) -o $@ $<
	@chmod 444 $@

.lsh.itxi:
	@$(RM) $@
	$(LIT2TEXI) -c $(LIT2TEXIFLAGS) -o $@ $<
	@chmod 444 $@

.lsh.ihtml:
	@$(RM) $@
	$(LIT2HTML) -c $(LIT2HTMLFLAGS) -o $@ $<
	@chmod 444 $@

.lsh.itex:
	@$(RM) $@
	$(LIT2LATEX) -c $(LIT2LATEXFLAGS) -o $@ $<
	@chmod 444 $@

.lc.c:
	@$(RM) $@
	$(LIT2PGM) $(LIT2PGMFLAGS) -o $@ $<
	@chmod 444 $@

.lc.itxi:
	@$(RM) $@
	$(LIT2TEXI) -c $(LIT2TEXIFLAGS) -o $@ $<
	@chmod 444 $@

.lc.ihtml:
	@$(RM) $@
	$(LIT2HTML) -c $(LIT2HTMLFLAGS) -o $@ $<
	@chmod 444 $@

.lc.itex:
	@$(RM) $@
	$(LIT2LATEX) -c $(LIT2LATEXFLAGS) -o $@ $<
	@chmod 444 $@

.lh.h:
	@$(RM) $@
	$(LIT2PGM) $(LIT2PGMFLAGS) -o $@ $<
	@chmod 444 $@

.lh.itxi:
	@$(RM) $@
	$(LIT2TEXI) -c $(LIT2TEXIFLAGS) -o $@ $<
	@chmod 444 $@

.lh.ihtml:
	@$(RM) $@
	$(LIT2HTML) -c $(LIT2HTMLFLAGS) -o $@ $<
	@chmod 444 $@

.lh.itex:
	@$(RM) $@
	$(LIT2LATEX) -c $(LIT2LATEXFLAGS) -o $@ $<
	@chmod 444 $@

.lflex.flex:
	@$(RM) $@
	$(LIT2PGM) $(LIT2PGMFLAGS) -o $@ $<
	@chmod 444 $@

.lflex.itxi:
	@$(RM) $@
	$(LIT2TEXI) -c $(LIT2TEXIFLAGS) -o $@ $<
	@chmod 444 $@

.lflex.ihtml:
	@$(RM) $@
	$(LIT2HTML) -c $(LIT2HTMLFLAGS) -o $@ $<
	@chmod 444 $@

.lflex.itex:
	@$(RM) $@
	$(LIT2LATEX) -c $(LIT2LATEXFLAGS) -o $@ $<
	@chmod 444 $@

endif

#-----------------------------------------------------------------------------
# C-related suffix rules

ifdef CSuffixRules

.s.o:
	@$(RM) $@
	$(AS) $(ASFLAGS) -o $@ $< || ( $(RM) $@ && exit 1 )

.c.o:
	@$(RM) $@
	$(CC) $(CFLAGS) -c $< -o $@

.c.s:
	@$(RM) $@
	$(CC) $(CFLAGS) -S $< -o $@

endif

ifdef FlexSuffixRules

.flex.c:
	@$(RM) $@
	$(FLEX) -t $(LFLAGS) $< > $@ || ( $(RM) $@ && exit 1 )

endif

#-----------------------------------------------------------------------------
# Yacc stuff

ifdef YaccSuffixRules

%.tab.c %.tab.h : %.y
	@$(RM) $*.tab.h $*.tab.c y.tab.c y.tab.h y.output
	$(YACC) $(YFLAGS) $<
	$(MV) y.tab.c $*.tab.c
	@chmod 444 $*.tab.c
	$(MV) y.tab.h $*.tab.h
	@chmod 444 $*.tab.h

endif

#-----------------------------------------------------------------------------
# Haskell Suffix Rules

# ToDo: these don't do the .hi-file games for hbc etc.

ifdef HaskellSuffixRules 
SuffixRule_o_hi		= YES
SuffixRule_lhs_o	= YES
SuffixRule_hs_o		= YES
endif

ifdef SuffixRule_lhs_o
%.o : %.hs
	$(HC) $(HCFLAGS) $($*_flags) -c $< -o $@
endif

ifdef SuffixRule_hs_o
%.o : %.lhs
	$(HC) $(HCFLAGS) $($*_flags) -c $< -o $@
endif

ifdef SuffixRule_lhs_hc
%.hc : %.lhs
	$(HC) $(HCFLAGS) $($*_flags) -C $< -o $@
endif

ifdef SuffixRule_hc_o
%.o : %.hc
	$(HC) $(HCFLAGS) $($*_flags) -c $< -o $@
endif

ifdef SuffixRule_o_hi
%.hi : %.o
	@if [ ! -f $@ ] ; then \
	    echo You need to create an initial $@ by hand ; \
	    exit 1; \
	else exit 0 ; \
	fi							
endif

ifdef SuffixRule_hc_hi
%.hi : %.hc
	@if [ ! -f $@ ] ; then \
	    echo You need to create an initial $@ by hand ; \
	    exit 1; \
	else exit 0 ; \
	fi
endif

#-----------------------------------------------------------------------------
# Runtest rules for calling $(GHC) on a single-file Haskell program

ifdef GhcRunTestRules

%.runtest : %.hs
	$(TIME) $(RUNSTDTEST) $(GHC) $(RUNSTDTEST_FLAGS) \
	  -o2 $*.stderr $($*_flags) $<

endif
