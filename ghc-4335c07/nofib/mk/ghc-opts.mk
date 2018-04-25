#################################################################################
#
#				opts.mk
#
#	This file defines Make variables for the
#	option flags for each utility program
#
# 	$Id: opts.mk,v 1.36 2004/08/26 20:08:54 panne Exp $
#
#################################################################################

#
# N.B. This is *NOT* the place to put extra options of ANY SORT!
#

# Exports:	Define P_OPTS for the most important utility programs, P, namely
#
#		AR AS CPP CTAGS C FLEX HC HSTAGS LD
#		MKDEPENDHS MKDEPENDLIT RUNTEST
#		UNLIT          


# For each such utility program P, this file defines
#
#	$(P)		The pathname to invoke the utility
#	P_OPTS		Options to pass to P
#
# P_OPTS is always defined like this:
#
# P_OPTS = SRC_P_OPTS WAY$(_way)_P_OPTS EXTRA_P_OPTS
#
# where the variables on the right hand side are set by the user or
# some other Makefile.  They have the following intended uses:
#
#	SRC_P_OPTS		Source-tree options for P
#	WAY$(_way)_P_OPTS	Source-tree options for P specific to $(way)
#	EXTRA_P_OPTS		Command-line options for P
#
# and for some programs
#
#	$(HcFlavour)_P_OPTS	Compiler-specific options for P
#       $($*_P_OPTS)            Target specific options for P
#
# All these options should be set with
#	thing += extra-options
# in case someone higher up the include hierarchy has already added some

# Which class of compiler are we aiming at?  (GHC, NHC or HUGS)
ifeq "$(HcFlavour)" ""
HcFlavour = GHC
endif

#################################################################################
#
#		Absolutely standard glue
#
#################################################################################

# All the standard gluing together, as in the comment right at the front


HC_OPTS            = $(BOOTSTRAPPING_PACKAGE_CONF_HC_OPTS) $(SRC_HC_OPTS) $(WAY$(_way)_HC_OPTS) $($*_HC_OPTS) $(EXTRA_HC_OPTS)
ifeq "$(HC_VERSION_GE_6_13)" "YES"
HC_OPTS           += -rtsopts
endif

HC_POST_OPTS       = $(SRC_HC_POST_OPTS) $(WAY$(_way)_HC_POST_OPTS) $($*_HC_POST_OPTS) $(EXTRA_HC_POST_OPTS)
HC_PRE_OPTS        = $(SRC_HC_PRE_OPTS) $(WAY$(_way)_HC_PRE_OPTS) $($*_HC_PRE_OPTS) $(EXTRA_HC_PRE_OPTS)

RUNTEST_OPTS       = $(SRC_RUNTEST_OPTS) $(WAY$(_way)_RUNTEST_OPTS) \
                     $($*_RUNTEST_OPTS) $(EXTRA_RUNTEST_OPTS)

ALEX_OPTS         = $(SRC_ALEX_OPTS) $($(HcFlavour)_ALEX_OPTS) $(WAY$(_way)_ALEX_OPTS) $($*_ALEX_OPTS) $(EXTRA_ALEX_OPTS)
AR_OPTS            = $(SRC_AR_OPTS) $(WAY$(_way)_AR_OPTS) $(EXTRA_AR_OPTS)
AS_OPTS            = $(SRC_AS_OPTS) $(WAY$(_way)_AS_OPTS) $(EXTRA_AS_OPTS)
CPP_OPTS           = $(SRC_CPP_OPTS) $(WAY$(_way)_CPP_OPTS) $(EXTRA_CPP_OPTS)
CC_OPTS            = $(SRC_CC_OPTS) $(WAY$(_way)_CC_OPTS) $($*_CC_OPTS) $(EXTRA_CC_OPTS)
FLEX_OPTS          = $(SRC_FLEX_OPTS) $(WAY$(_way)_FLEX_OPTS) $(EXTRA_FLEX_OPTS)
HAPPY_OPTS         = $(SRC_HAPPY_OPTS) $($(HcFlavour)_HAPPY_OPTS) $(WAY$(_way)_HAPPY_OPTS) $($*_HAPPY_OPTS) $(EXTRA_HAPPY_OPTS)
GC_OPTS            = $(SRC_GC_OPTS) $(WAY$(_way)_GC_OPTS) $($*_GC_OPTS) $(EXTRA_GC_OPTS)
HSTAGS_OPTS        = $(SRC_HSTAGS_OPTS) $(WAY$(_way)_HSTAGS_OPTS) $(EXTRA_HSTAGS_OPTS)
HSC2HS_OPTS        = $(SRC_HSC2HS_OPTS) $($(HcFlavour)_HSC2HS_OPTS) $(WAY$(_way)_HSC2HS_OPTS) $(EXTRA_HSC2HS_OPTS)
LD_OPTS            = $(SRC_LD_OPTS) $(WAY$(_way)_LD_OPTS) $(EXTRA_LD_OPTS)
MKDEPENDHS_OPTS    = $(BOOTSTRAPPING_PACKAGE_CONF_MKDEPENDHS_OPTS) \
                     $(SRC_MKDEPENDHS_OPTS) $(WAY$(_way)_MKDEPENDHS_OPTS) \
                     $(EXTRA_MKDEPENDHS_OPTS)
MKDEPENDLIT_OPTS   = $(SRC_MKDEPENDLIT_OPTS) $(WAY$(_way)_MKDEPENDLIT_OPTS) \
                     $(EXTRA_MKDEPENDLIT_OPTS)
XSLTPROC_OPTS      = $(WAY$(_way)_XSLTPROC_OPTS) $(EXTRA_XSLTPROC_OPTS)
UNLIT_OPTS         = $(SRC_UNLIT_OPTS) $(WAY$(_way)_UNLIT_OPTS) $(EXTRA_UNLIT_OPTS)

# Version of CC_OPTS to use when GHC is the C compiler
GHC_CC_OPTS 	   = $(addprefix -optc, $(CC_OPTS)) $(HC_OPTS)
