TOP=../..
include $(TOP)/mk/boilerplate.mk

#
# You'll only get this with Parallel Haskell or 
# GranSim..
#
ifeq "$(BuildingParallel)" "YES"
INSTALL_PARALLEL_SCRIPTS = YES
endif
ifeq "$(BuildingGranSim)" "YES"
INSTALL_PARALLEL_SCRIPTS = YES
endif

ifeq "$(INSTALL_PARALLEL_SCRIPTS)" "YES"

PERL_PROGS = \
  grs2gr gr2qp qp2ps ghc-fool-sort ghc-unfool-sort gr2pe gr2java \
  qp2ap gr2RTS RTS2gran gran-extr gp-ext-imp tf avg-RTS SPLIT \
  AVG SN get_SN sn_filter ps-scale-y

BASH_PROGS = gr2ps gr2jv gr2ap gr2gran

else

PERL_PROGS =
BASH_PROGS =

endif

PERL_INSTALLERS = $(addprefix install.perl.,$(PERL_PROGS))
BASH_INSTALLERS = $(addprefix install.bash.,$(BASH_PROGS))

.PHONY: install.perl.%
.PHONY: install.bash.%

install :: $(PERL_INSTALLERS) $(BASH_INSTALLERS)

$(PERL_INSTALLERS): install.perl.%:
	rm -f               $(bindir)/$*
	echo "#!$(PERL)" >> $(bindir)/$*
	cat $*.pl        >> $(bindir)/$*
	$(EXECUTABLE_FILE)  $(bindir)/$*

$(BASH_INSTALLERS): install.bash.%:
	rm -f               $(bindir)/$*
	echo "#!$(BASH)" >> $(bindir)/$*
	cat $*.bash      >> $(bindir)/$*
	$(EXECUTABLE_FILE)  $(bindir)/$*

include $(TOP)/mk/bindist.mk

include $(TOP)/mk/target.mk
