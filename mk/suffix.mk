#################################################################################
#
#			    suffix.mk
#
#		Suffix rules for fptools
#
#################################################################################

# 
# This file contain the default suffix rules for all the fptools projects.
#


# No need to define .SUFFIXES because we don't use any suffix rules
# Instead we use gmake's pattern rules exlusively

.SUFFIXES:

# However, if $(way) is set then we have to define $(way_) and $(_way)
# from it in the obvious fashion.
# This must be done here (or earlier), but not in target.mk with the other
# way management, because the pattern rules in this file take a snapshot of
# the value of $(way_) and $(_way), and it's no good setting them later!

ifneq "$(way)" ""
  way_ := $(way)_
  _way := _$(way)
endif

#-----------------------------------------------------------------------------
# Haskell Suffix Rules

HASKELL_SPLIT_PRE= \
 $(RM) $@ ; if [ ! -d $(basename $@) ]; then mkdir $(basename $@) ; else exit 0; fi; \
 find $(basename $@) -name '*.$(way_)o' -print | xargs $(RM) __rm_food;
HASKELL_SPLIT_POST= touch $@
HASKELL_PRE_COMPILE=$(patsubst %,$(HASKELL_SPLIT_PRE),$(filter -split-objs,$(HC_OPTS)))
HASKELL_POST_COMPILE=$(patsubst %,$(HASKELL_SPLIT_POST),$(filter -split-objs,$(HC_OPTS)))

%.$(way_)o : %.hs
	$(HASKELL_PRE_COMPILE)
	$(HC) $(HC_OPTS) -c $< -o $@ -osuf $(subst .,,$(suffix $@))
	$(HASKELL_POST_COMPILE)
			 
%.$(way_)o : %.lhs	 
	$(HASKELL_PRE_COMPILE)
	$(HC) $(HC_OPTS) -c $< -o $@ -osuf $(subst .,,$(suffix $@))
	$(HASKELL_POST_COMPILE)
			 
%.$(way_)hc : %.lhs	 
	$(RM) $@
	$(HC) $(HC_OPTS) -C $< -o $@
			 
%.$(way_)o : %.$(way_)hc 
	$(HASKELL_PRE_COMPILE)
	$(HC) $(HC_OPTS) -c $< -o $@ -osuf $(subst .,,$(suffix $@))
	$(HASKELL_POST_COMPILE)

%.$(way_)hc : %.lhc
	@$(RM) $@
	$(UNLIT) $< $@
	@chmod 444 $@


# Here's an interesting rule!
# The .hi file depends on the .o file,
# so if the .hi file is dated earlier than the .o file (commonly the case,
# when interfaces are stable) this rule just makes sure that the .o file,
# is up to date.  Then it does nothing to generate the .hi file from the 
# .o file, because the act of making sure the .o file is up to date also
# updates the .hi file (if necessary).

%.$(way_)hi : %.$(way_)o
	@if [ ! -f $@ ] ; then \
	    echo Panic! $< exists, but $@ does not. \
	    exit 1; \
	else exit 0 ; \
	fi							

%.$(way_)hi : %.$(way_)hc
	@if [ ! -f $@ ] ; then \
	    echo Panic! $< exists, but $@ does not. \
	    exit 1; \
	else exit 0 ; \
	fi

#-----------------------------------------------------------------------------
# Happy Suffix Rules
#
.PRECIOUS: %.hs

%.hs : %.ly
	$(HAPPY) $(HAPPY_OPTS) -g $<

#-----------------------------------------------------------------------------
# Lx Suffix Rules
#

%.hs : %.lx
	$(LX) $(LX_OPTS) $<

#-----------------------------------------------------------------------------
# C-related suffix rules

%.$(way_)o : %.$(way_)s
	@$(RM) $@
	$(AS) $(AS_OPTS) -o $@ $< || ( $(RM) $@ && exit 1 )

%.$(way_)o : %.c
	@$(RM) $@
	$(CC) $(CC_OPTS) -c $< -o $@

#%.$(way_)s : %.c
#	@$(RM) $@
#	$(CC) $(CC_OPTS) -S $< -o $@

%.c : %.flex
	@$(RM) $@
	$(FLEX) -t $(FLEX_OPTS) $< > $@ || ( $(RM) $@ && exit 1 )
%.c : %.lex
	@$(RM) $@
	$(FLEX) -t $(FLEX_OPTS) $< > $@ || ( $(RM) $@ && exit 1 )

#-----------------------------------------------------------------------------
# Yacc stuff

%.tab.c %.tab.h : %.y
	@$(RM) $*.tab.h $*.tab.c y.tab.c y.tab.h y.output
	$(YACC) $(YACC_OPTS) $<
	$(MV) y.tab.c $*.tab.c
	@chmod 444 $*.tab.c
	$(MV) y.tab.h $*.tab.h
	@chmod 444 $*.tab.h


#-----------------------------------------------------------------------------
# Runtest rules for calling $(HC) on a single-file Haskell program

%.hs : %.runtest
	$(TIME) $(RUNTEST) $(HC) $(RUNTEST_FLAGS) -o2 $*.stderr $<


#-----------------------------------------------------------------------------
# Doc processing suffix rules

%.dvi : %.tex
	@$(RM) $@
	$(LTX) $<

%.ps : %.dvi
	@$(RM) $@
	dvips $< -o $@

%.tex : %.verb
	@$(RM) $*.tex
	expand $*.verb | $(VERBATIM) > $*.tex

%.tex : %.tib
	@$(RM) $*.tex $*.verb-t.tex
	$(TIB) $*.tib
	expand $*.tib-t.tex | $(VERBATIM) > $*.tex
	@$(RM) $*.tib-t.tex

%.ps : %.fig
	@$(RM) $@
	fig2dev -L ps $< $@

%.tex : %.fig
	@$(RM) $@
	fig2dev -L latex $< $@

#-----------------------------------------------------------------------------
# Literate suffix rules

# ToDo: somehow macroize this lot. (if only!)

%.itxi : %.lit
	@$(RM) $@
	$(LIT2TEXI) -c $(LIT2TEXI_OPTS) -o $@ $<
	@chmod 444 $@

%.txt : %.lit
	@$(RM) $@
	$(LIT2TEXT) $(LIT2TEXT_OPTS) -o $@ $<
	@chmod 444 $@

%.ihtml : %.lit
	@$(RM) $@
	$(LIT2HTML) -c $(LIT2HTML_OPTS) -o $@ $<
	@chmod 444 $@

%.itex : %.lit
	@$(RM) $@
	$(LIT2LATEX) -c $(LIT2LATEX_OPTS) -o $@ $<
	@chmod 444 $@

#
# Produce stand-alone TEX documents
#
%.tex : %.itex
	@$(RM) $@
	$(LIT2LATEX) -S $(LIT2LATEX_OPTS) -o $@ $<
	@chmod 444 $@

%.tex : %.lhs
	@$(RM) $@
	$(LIT2LATEX) -S $(LIT2LATEX_OPTS) -o $@ $<
	@chmod 444 $@

%.texi : %.lhs
	@$(RM) $@
	$(LIT2TEXI) -S $(LIT2TEXI_OPTS) -o $@ $<
	@chmod 444 $@

%.html : %.lhs
	@$(RM) $@ 
	$(LIT2TEXI) $(LIT2TEXI_OPTS) -o $(patsubst %.html,%.texi,$@) $<
	$(TEXI2HTML) $(TEXI2HTML_OPTS) $(patsubst %.lhs,%.texi,$<) 
	@touch $@

%.info:: %.texi
	@$(RM) $@
	$(MAKEINFO) $(MAKEINFO_OPTS) $< && $(POSTMAKEINFO) $@

%.hs : %.lhs
	@$(RM) $@
	$(LIT2PGM) $(LIT2PGM_OPTS) -o $@ $<
	@chmod 444 $@

%.itxi : %.lhs
	@$(RM) $@
	$(LIT2TEXI) -c $(LIT2TEXI_OPTS) -o $@ $<
	@chmod 444 $@

%.ihtml : %.lhs
	@$(RM) $@
	$(LIT2HTML) -c $(LIT2HTML_OPTS) -o $@ $<
	@chmod 444 $@

%.itex : %.lhs
	@$(RM) $@
	$(LIT2LATEX) -c $(LIT2LATEX_OPTS) -o $@ $<
	@chmod 444 $@

%.tex : %.lhs
	$(LIT2LATEX) -S -c $(LIT2LATEX_OPTS) -o $@ $<
	$(HC) $(HC_OPTS) -c $< -o $@


%.itxi : %.lhc
	@$(RM) $@
	$(LIT2TEXI) -c $(LIT2TEXI_OPTS) -o $@ $<
	@chmod 444 $@

%.ihtml : %.lhc
	@$(RM) $@
	$(LIT2HTML) -c $(LIT2HTML_OPTS) -o $@ $<
	@chmod 444 $@

%.itex : %.lhc
	@$(RM) $@
	$(LIT2LATEX) -c $(LIT2LATEX_OPTS) -o $@ $<
	@chmod 444 $@

#
# Temporary, until either unlit is lifted out of ghc/
# or literate is properly set up locally -- SOF
#
%.prl : %.lprl
	@$(RM) $@
	$(UNLIT) $(UNLIT_OPTS) $< $@
	@chmod 444 $@

%.itxi : %.lprl
	@$(RM) $@
	$(LIT2TEXI) -c $(LIT2TEXI_OPTS) -o $@ $<
	@chmod 444 $@

%.ihtml : %.lprl
	@$(RM) $@
	$(LIT2HTML) -c $(LIT2HTML_OPTS) -o $@ $<
	@chmod 444 $@

%.itex : %.lprl
	@$(RM) $@
	$(LIT2LATEX) -c $(LIT2LATEX_OPTS) -o $@ $<
	@chmod 444 $@

%.sh : %.lsh
	@$(RM) $@
	$(LIT2PGM) $(LIT2PGM_OPTS) -o $@ $<
	@chmod 444 $@

%.itxi : %.lsh
	@$(RM) $@
	$(LIT2TEXI) -c $(LIT2TEXI_OPTS) -o $@ $<
	@chmod 444 $@

%.ihtml : %.lsh
	@$(RM) $@
	$(LIT2HTML) -c $(LIT2HTML_OPTS) -o $@ $<
	@chmod 444 $@

%.itex : %.lsh
	@$(RM) $@
	$(LIT2LATEX) -c $(LIT2LATEX_OPTS) -o $@ $<
	@chmod 444 $@

%.c : %.lc
	@$(RM) $@
	$(UNLIT) $< $@
	@chmod 444 $@

%.itxi : %.lc
	@$(RM) $@
	$(LIT2TEXI) -c $(LIT2TEXI_OPTS) -o $@ $<
	@chmod 444 $@

%.ihtml : %.lc
	@$(RM) $@
	$(LIT2HTML) -c $(LIT2HTML_OPTS) -o $@ $<
	@chmod 444 $@

%.itex : %.lc
	@$(RM) $@
	$(LIT2LATEX) -c $(LIT2LATEX_OPTS) -o $@ $<
	@chmod 444 $@

%.h : %.lh
	@$(RM) $@
	$(UNLIT) $< $@
	@chmod 444 $@

%.itxi : %.lh
	@$(RM) $@
	$(LIT2TEXI) -c $(LIT2TEXI_OPTS) -o $@ $<
	@chmod 444 $@

%.ihtml : %.lh
	@$(RM) $@
	$(LIT2HTML) -c $(LIT2HTML_OPTS) -o $@ $<
	@chmod 444 $@

%.itex : %.lh
	@$(RM) $@
	$(LIT2LATEX) -c $(LIT2LATEX_OPTS) -o $@ $<
	@chmod 444 $@

%.flex : %.lflex
	@$(RM) $@
	$(LIT2PGM) $(LIT2PGM_OPTS) -o $@ $<
	@chmod 444 $@

%.itxi : %.lflex
	@$(RM) $@
	$(LIT2TEXI) -c $(LIT2TEXI_OPTS) -o $@ $<
	@chmod 444 $@

%.ihtml : %.lflex
	@$(RM) $@
	$(LIT2HTML) -c $(LIT2HTML_OPTS) -o $@ $<
	@chmod 444 $@

%.itex : %.lflex
	@$(RM) $@
	$(LIT2LATEX) -c $(LIT2LATEX_OPTS) -o $@ $<
	@chmod 444 $@

