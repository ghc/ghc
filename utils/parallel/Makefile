TOP=../..
include $(TOP)/mk/boilerplate.mk

PERL_PROGS = \
  grs2gr gr2qp qp2ps ghc-fool-sort ghc-unfool-sort gr2pe gr2java \
  qp2ap gr2RTS RTS2gran gran-extr gp-ext-imp tf avg-RTS SPLIT \
  AVG SN get_SN sn_filter ps-scale-y


BASH_PROGS = gr2ps gr2jv gr2ap gr2gran

#
# One rule fits all, not particularly selective.
#
$(PERL_PROGS) : $(patsubst %,%.pl,$(PERL_PROGS))
$(BASH_PROGS) : $(patsubst %,%.bash,$(BASH_PROGS))


all :: $(PERL_PROGS) $(BASH_PROGS)

$(PERL_PROGS) :
	$(RM) $@
	@echo Creating $@...
	@echo "#!"$(PERL) > $@
	@cat $@.pl >> $@
	@chmod a+x $@

$(BASH_PROGS) :
	$(RM) $@
	@echo Creating $@...
	@echo "#!"$(BASH) > $@
	@cat $@.bash >> $@
	@chmod a+x $@

#
# You'll only get this with Parallel Haskell or 
# GranSim..
#
ifeq "$(BuildingParallel)" "YES"
INSTALL_SCRIPTS += $(BASH_PROGS) $(PERL_PROGS)
else
ifeq "$(BuildingGranSim)" "YES"
INSTALL_SCRIPTS += $(BASH_PROGS) $(PERL_PROGS)
endif
endif

CLEAN_FILES     += $(BASH_PROGS) $(PERL_PROGS)

include $(TOP)/mk/target.mk
