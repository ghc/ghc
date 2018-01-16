#################################################################################
#
#			    nofib/mk/opts.mk
#
# 	$Id: opts.mk,v 1.8 2002/05/20 12:26:25 simonmar Exp $
#
#################################################################################

# The default definition of RUNTEST_OPTS in $(TOP)/mk/opts.mk assume
# that it is going to be used in a pattern rule. This not the case
# for NoFib tests, so we define a custom version of RUNTEST_OPTS
# that instead of $* uses $(NOFIB_PROG), so as to provide a way
# to configure (and override) the options to run a particular test
# with.
RUNTEST_OPTS       = $(SRC_RUNTEST_OPTS) $(WAY$(_way)_RUNTEST_OPTS) \
                     $($(NOFIB_PROG)_RUNTEST_OPTS) $(EXTRA_RUNTEST_OPTS)

ifneq "$(way)" "mp"
# if testing GUM don't generate a -S style log file; it may well differ 
SRC_RUNTEST_OPTS += -ghc-timing 
endif
# SRC_RUNTEST_OPTS += +RTS -H10m -K10m -RTS

#-----------------------------------------------------------------------------
# Setting for Haskell compiler
#
SRC_HC_OPTS  += -H32m -hisuf $(way_)hi

#mode = "slow"

ifeq "$(mode)" "slow"
 PROG_ARGS = $(SLOW_OPTS)
else
 ifeq "$(mode)" "fast"
  PROG_ARGS = $(FAST_OPTS)
 else
  PROG_ARGS = $(NORM_OPTS)
 endif
endif

# Hmm, we shouldn't have to do this.
BOOTSTRAPPING_PACKAGE_CONF_MKDEPENDHS_OPTS=
BOOTSTRAPPING_PACKAGE_CONF_HC_OPTS=
