#################################################################################
#
#			    ghc/tests/mk/boilerplate.mk
#
#		Boilerplate Makefile for an fptools project
#
#################################################################################

# Begin by slurping in the boilerplate from one level up, 
# with standard TOP-mangling
# Remember, TOP is the top level of the innermost level
# ( FPTOOLS_TOP, which will be set while processing
#   toplevel boilerplate, is the fptools top )

TEST_TOP := $(TOP)
TOP := $(TOP)/..

include $(TOP)/mk/boilerplate.mk
TOP:=$(TEST_TOP)

# By default, we're testing the GHC in the build tree.
HC=$(GHC_INPLACE)

# -----------------------------------------------------------------
# Everything after this point
# augments or overrides previously set variables.
# -----------------------------------------------------------------

-include $(TOP)/mk/paths.mk
-include $(TOP)/mk/opts.mk
-include $(TOP)/mk/version.mk
-include $(TOP)/mk/suffix.mk
