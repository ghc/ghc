#################################################################################
#
#				opts.mk
#
#	This file defines Make variables for the
#	option flags for each utility program
#
# 	$Id: opts.mk,v 1.9 1999/02/02 14:16:28 sof Exp $
#
#################################################################################

# Exports:	Define P_OPTS for the most important utility programs, P, namely
#
#		AR AS CPP CTAGS C FLEX HC HSTAGS LD LINT 
#		LIT2CHANGELOG LIT2HTML LIT2LATEX LIT2PGM LIT2TEXI
#		MKDEPENDC MKDEPENDHS MKDEPENDLIT MSUB
#		RUNTEST UGEN UNLIT YACC          


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
#       $($*_P_OPTS)            Target specific options for P
#
# All these options should be set with
#	thing += extra-options
# in case someone higher up the include hierarchy has already added some


#################################################################################
#
#		Global option flags for utilities
#
#################################################################################

# These flags make flex 8-bit
SRC_FLEX_OPTS	+= -8

SRC_INSTALL_BIN_OPTS	+= -s

# lint gets all CPP's flags too
SRC_LINT_OPTS		+= -axz -DLINT $(SRC_CPP_OPTS)
WAY$(_way)_LINT_OPTS	+= WAY$(_way)_CPP_OPTS


# Default fptools options for texi2html:
#
#  - each node in separate file.
#  - handle texi menus and glossaries.
#
# 
SRC_TEXI2HTML_OPTS +=-menu -verbose -glossary


#################################################################################
#
#		Absolutely standard glue
#
#################################################################################

# All the standard gluing together, as in the comment right at the front


HC_OPTS            = $(SRC_HC_OPTS) $(WAY$(_way)_HC_OPTS) $($*_HC_OPTS) $(EXTRA_HC_OPTS)
HC_POST_OPTS       = $(SRC_HC_POST_OPTS) $(WAY$(_way)_HC_POST_OPTS) $($*_HC_POST_OPTS) $(EXTRA_HC_POST_OPTS)
HC_PRE_OPTS        = $(SRC_HC_PRE_OPTS) $(WAY$(_way)_HC_PRE_OPTS) $($*_HC_PRE_OPTS) $(EXTRA_HC_PRE_OPTS)

RUNTEST_OPTS       = $(SRC_RUNTEST_OPTS) $(WAY$(_way)_RUNTEST_OPTS) \
                     $($*_RUNTEST_OPTS) $(EXTRA_RUNTEST_OPTS)

AR_OPTS            = $(SRC_AR_OPTS) $(WAY$(_way)_AR_OPTS) $(EXTRA_AR_OPTS)
AS_OPTS            = $(SRC_AS_OPTS) $(WAY$(_way)_AS_OPTS) $(EXTRA_AS_OPTS)
CPP_OPTS           = $(SRC_CPP_OPTS) $(WAY$(_way)_CPP_OPTS) $(EXTRA_CPP_OPTS)
CTAGS_OPTS         = $(SRC_CTAGS_OPTS) $(WAY$(_way)_CTAGS_OPTS) $(EXTRA_CTAGS_OPTS)
CC_OPTS            = $(SRC_CC_OPTS) $(WAY$(_way)_CC_OPTS) $($*_CC_OPTS) $(EXTRA_CC_OPTS)
FLEX_OPTS          = $(SRC_FLEX_OPTS) $(WAY$(_way)_FLEX_OPTS) $(EXTRA_FLEX_OPTS)
HAPPY_OPTS         = $(SRC_HAPPY_OPTS) $(WAY$(_way)_HAPPY_OPTS) $($*_HAPPY_OPTS) $(EXTRA_HAPPY_OPTS)
GREENCARD_OPTS     = $(SRC_GREENCARD_OPTS) $(WAY$(_way)_GREENCARD_OPTS) $($*_GREENCARD_OPTS) $(EXTRA_GREENCARD_OPTS)
HSTAGS_OPTS        = $(SRC_HSTAGS_OPTS) $(WAY$(_way)_HSTAGS_OPTS) $(EXTRA_HSTAGS_OPTS)
INSTALL_OPTS       = $(SRC_INSTALL_OPTS) $(WAY$(_way)_INSTALL_OPTS) $(EXTRA_INSTALL_OPTS)
INSTALL_BIN_OPTS   = $(INSTALL_OPTS) $(SRC_INSTALL_BIN_OPTS)
LD_OPTS            = $(SRC_LD_OPTS) $(WAY$(_way)_LD_OPTS) $(EXTRA_LD_OPTS)
LINT_OPTS          = $(SRC_LINT_OPTS) $(WAY$(_way)_LINT_OPTS) $(EXTRA_LINT_OPTS)
LIT2CHANGELOG_OPTS = $(SRC_LIT2CHANGELOG_OPTS) $(WAY$(_way)_LIT2CHANGELOG_OPTS) \
                     $(EXTRA_LIT2CHANGELOG_OPTS)
LIT2HTML_OPTS      = $(SRC_LIT2HTML_OPTS) $(WAY$(_way)_LIT2HTML_OPTS) $(EXTRA_LIT2HTML_OPTS)
LIT2LATEX_OPTS     = $(SRC_LIT2LATEX_OPTS) $(WAY$(_way)_LIT2LATEX_OPTS) $(EXTRA_LIT2LATEX_OPTS)
LIT2PGM_OPTS       = $(SRC_LIT2PGM_OPTS) $(WAY$(_way)_LIT2PGM_OPTS) $(EXTRA_LIT2PGM_OPTS)
LIT2TEXI_OPTS      = $(SRC_LIT2TEXI_OPTS) $(WAY$(_way)_LIT2TEXI_OPTS) $(EXTRA_LIT2TEXI_OPTS)
TEXI2HTML_OPTS     = $(SRC_TEXI2HTML_OPTS) $(WAY$(_way)_TEXI2HTML_OPTS) $(EXTRA_TEXI2HTML_OPTS)
MKDEPENDC_OPTS     = $(SRC_MKDEPENDC_OPTS) $(WAY$(_way)_MKDEPENDC_OPTS) $(EXTRA_MKDEPENDC_OPTS)
MKDEPENDHS_OPTS    = $(SRC_MKDEPENDHS_OPTS) $(WAY$(_way)_MKDEPENDHS_OPTS) \
                     $(EXTRA_MKDEPENDHS_OPTS)
MKDEPENDLIT_OPTS   = $(SRC_MKDEPENDLIT_OPTS) $(WAY$(_way)_MKDEPENDLIT_OPTS) \
                     $(EXTRA_MKDEPENDLIT_OPTS)
SGML2LATEX_OPTS    = $(SRC_SGML2LATEX_OPTS) $(WAY$(_way)_SGML2LATEX_OPTS) $(EXTRA_SGML2LATEX_OPTS)
SGML2INFO_OPTS     = $(SRC_SGML2INFO_OPTS) $(WAY$(_way)_SGML2INFO_OPTS) $(EXTRA_INFO_OPTS)
SGML2TXT_OPTS      = $(SRC_SGML2TXT_OPTS) $(WAY$(_way)_SGML2TXT_OPTS) $(EXTRA_SGML2TXT_OPTS)
SGML2HTML_OPTS     = $(SRC_SGML2HTML_OPTS) $(WAY$(_way)_SGML2HTML_OPTS) $(EXTRA_SGML2HTML_OPTS)
UGEN_OPTS          = $(SRC_UGEN_OPTS) $(WAY$(_way)_UGEN_OPTS) $(EXTRA_UGEN_OPTS)
UNLIT_OPTS         = $(SRC_UNLIT_OPTS) $(WAY$(_way)_UNLIT_OPTS) $(EXTRA_UNLIT_OPTS)
YACC_OPTS          = $(SRC_YACC_OPTS) $(WAY$(_way)_YACC_OPTS) $(EXTRA_YACC_OPTS)
ZIP_OPTS           = $(SRC_ZIP_OPTS) $(EXTRA_ZIP_OPTS)
