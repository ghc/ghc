#################################################################################
#
#				opts.mk
#
#	This file defines Make variables for the
#	option flags for each utility program
#
# 	$Id: opts.mk,v 1.34 2003/08/18 15:45:09 panne Exp $
#
#################################################################################

#
# N.B. This is *NOT* the place to put extra options of ANY SORT!
#

# Exports:	Define P_OPTS for the most important utility programs, P, namely
#
#		AR AS CPP CTAGS C FLEX HC HSTAGS LD LINT 
#		LIT2CHANGELOG LIT2HTML LIT2LATEX LIT2PGM
#		MKDEPENDC MKDEPENDHS MKDEPENDLIT RUNTEST
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


HC_OPTS            = $(SRC_HC_OPTS) $(WAY$(_way)_HC_OPTS) $($*_HC_OPTS) $(EXTRA_HC_OPTS)
HC_POST_OPTS       = $(SRC_HC_POST_OPTS) $(WAY$(_way)_HC_POST_OPTS) $($*_HC_POST_OPTS) $(EXTRA_HC_POST_OPTS)
HC_PRE_OPTS        = $(SRC_HC_PRE_OPTS) $(WAY$(_way)_HC_PRE_OPTS) $($*_HC_PRE_OPTS) $(EXTRA_HC_PRE_OPTS)

ILX2IL_OPTS        = $(SRC_ILX2IL_OPTS) $(WAY$(_way)_ILX2IL_OPTS) $($*_ILX2IL_OPTS) $(EXTRA_ILX2IL_OPTS)
ILASM_OPTS         = $(SRC_ILASM_OPTS) $(WAY$(_way)_ILASM_OPTS) $($*_ILASM_OPTS) $(EXTRA_ILASM_OPTS)

RUNTEST_OPTS       = $(SRC_RUNTEST_OPTS) $(WAY$(_way)_RUNTEST_OPTS) \
                     $($*_RUNTEST_OPTS) $(EXTRA_RUNTEST_OPTS)

ALEX_OPTS         = $(SRC_ALEX_OPTS) $($(HcFlavour)_ALEX_OPTS) $(WAY$(_way)_ALEX_OPTS) $($*_ALEX_OPTS) $(EXTRA_ALEX_OPTS)
AR_OPTS            = $(SRC_AR_OPTS) $(WAY$(_way)_AR_OPTS) $(EXTRA_AR_OPTS)
AS_OPTS            = $(SRC_AS_OPTS) $(WAY$(_way)_AS_OPTS) $(EXTRA_AS_OPTS)
BLD_DLL_OPTS       = $(SRC_BLD_DLL_OPTS) $(WAY$(_way)_BLD_DLL_OPTS) $($*_HC_OPTS) $(EXTRA_BLD_DLL_OPTS)
CPP_OPTS           = $(SRC_CPP_OPTS) $(WAY$(_way)_CPP_OPTS) $(EXTRA_CPP_OPTS)
CTAGS_OPTS         = $(SRC_CTAGS_OPTS) $(WAY$(_way)_CTAGS_OPTS) $(EXTRA_CTAGS_OPTS)
CC_OPTS            = $(SRC_CC_OPTS) $(WAY$(_way)_CC_OPTS) $($*_CC_OPTS) $(EXTRA_CC_OPTS)
FLEX_OPTS          = $(SRC_FLEX_OPTS) $(WAY$(_way)_FLEX_OPTS) $(EXTRA_FLEX_OPTS)
HADDOCK_OPTS       = $(SRC_HADDOCK_OPTS) $(WAY$(_way)_HADDOCK_OPTS) $($*_HADDOCK_OPTS) $(EXTRA_HADDOCK_OPTS)
HAPPY_OPTS         = $(SRC_HAPPY_OPTS) $($(HcFlavour)_HAPPY_OPTS) $(WAY$(_way)_HAPPY_OPTS) $($*_HAPPY_OPTS) $(EXTRA_HAPPY_OPTS)
GC_OPTS            = $(SRC_GC_OPTS) $(WAY$(_way)_GC_OPTS) $($*_GC_OPTS) $(EXTRA_GC_OPTS)
HSTAGS_OPTS        = $(SRC_HSTAGS_OPTS) $(WAY$(_way)_HSTAGS_OPTS) $(EXTRA_HSTAGS_OPTS)
HSC2HS_OPTS        = $(SRC_HSC2HS_OPTS) $($(HcFlavour)_HSC2HS_OPTS) $(WAY$(_way)_HSC2HS_OPTS) $(EXTRA_HSC2HS_OPTS)
INSTALL_OPTS       = $(SRC_INSTALL_OPTS) $(WAY$(_way)_INSTALL_OPTS) $(EXTRA_INSTALL_OPTS)
INSTALL_BIN_OPTS   = $(INSTALL_OPTS) $(SRC_INSTALL_BIN_OPTS)
LD_OPTS            = $(SRC_LD_OPTS) $(WAY$(_way)_LD_OPTS) $(EXTRA_LD_OPTS)
LINT_OPTS          = $(SRC_LINT_OPTS) $(WAY$(_way)_LINT_OPTS) $(EXTRA_LINT_OPTS)
HEVEA_OPTS         = $(SRC_HEVEA_OPTS) $(WAY$(_way)_HEVEA_OPTS) $(EXTRA_HEVEA_OPTS)
HACHA_OPTS         = $(SRC_HACHA_OPTS) $(WAY$(_way)_HACHA_OPTS) $(EXTRA_HACHA_OPTS)
LIT2CHANGELOG_OPTS = $(SRC_LIT2CHANGELOG_OPTS) $(WAY$(_way)_LIT2CHANGELOG_OPTS) \
                     $(EXTRA_LIT2CHANGELOG_OPTS)
LIT2HTML_OPTS      = $(SRC_LIT2HTML_OPTS) $(WAY$(_way)_LIT2HTML_OPTS) $(EXTRA_LIT2HTML_OPTS)
LIT2LATEX_OPTS     = $(SRC_LIT2LATEX_OPTS) $(WAY$(_way)_LIT2LATEX_OPTS) $(EXTRA_LIT2LATEX_OPTS)
LIT2PGM_OPTS       = $(SRC_LIT2PGM_OPTS) $(WAY$(_way)_LIT2PGM_OPTS) $(EXTRA_LIT2PGM_OPTS)
MKDEPENDC_OPTS     = $(SRC_MKDEPENDC_OPTS) $(WAY$(_way)_MKDEPENDC_OPTS) $(EXTRA_MKDEPENDC_OPTS)
MKDEPENDHS_OPTS    = $(SRC_MKDEPENDHS_OPTS) $(WAY$(_way)_MKDEPENDHS_OPTS) \
                     $(EXTRA_MKDEPENDHS_OPTS)
MKDEPENDLIT_OPTS   = $(SRC_MKDEPENDLIT_OPTS) $(WAY$(_way)_MKDEPENDLIT_OPTS) \
                     $(EXTRA_MKDEPENDLIT_OPTS)
SGML2DVI_OPTS      = $(SRC_SGML2DVI_OPTS) $(WAY$(_way)_SGML2DVI_OPTS) $(EXTRA_SGML2DVI_OPTS)
SGML2PS_OPTS       = $(SRC_SGML2PS_OPTS) $(WAY$(_way)_SGML2PS_OPTS) $(EXTRA_SGML2PS_OPTS)
SGML2PDF_OPTS      = $(SRC_SGML2PDF_OPTS) $(WAY$(_way)_SGML2PDF_OPTS) $(EXTRA_SGML2PDF_OPTS)
SGML2RTF_OPTS      = $(SRC_SGML2RTF_OPTS) $(WAY$(_way)_SGML2RTF_OPTS) $(EXTRA_SGML2RTF_OPTS)
SGML2HTML_OPTS     = $(SRC_SGML2HTML_OPTS) $(WAY$(_way)_SGML2HTML_OPTS) $(EXTRA_SGML2HTML_OPTS)
UNLIT_OPTS         = $(SRC_UNLIT_OPTS) $(WAY$(_way)_UNLIT_OPTS) $(EXTRA_UNLIT_OPTS)
ZIP_OPTS           = $(SRC_ZIP_OPTS) $(EXTRA_ZIP_OPTS)

# Version of CC_OPTS to use when GHC is the C compiler
GHC_CC_OPTS 	   = $(addprefix -optc, $(CC_OPTS)) $(HC_OPTS)
