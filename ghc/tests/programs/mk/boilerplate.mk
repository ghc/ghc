#################################################################################
#
#			    ghc/tests/programs/mk/boilerplate.mk
#
#		Boilerplate Makefile for an fptools project
#
#################################################################################

# Begin by slurping in the boilerplate from one level up, 
# with standard TOP-mangling
# Remember, TOP is the top level of the innermost level
# ( FPTOOLS_TOP, which will be set while processing
#   toplevel boilerplate, is the fptools top )

TEST_PROGRAMS_TOP := $(TOP)
TOP := $(TOP)/..
include $(TOP)/mk/boilerplate.mk
TOP:=$(TEST_PROGRAMS_TOP)


# -----------------------------------------------------------------
# Everything after this point
# augments or overrides previously set variables.
# (these files are optional, so `make' won't fret if it
#  cannot get to them).
# -----------------------------------------------------------------


# Define TEST_PROG.  In ..../nofib/imaginary/exp3_8, PROG is exp3_8 by default.
#
TEST_PROG = $(notdir $(shell pwd))$(_way)

# Eventually, have the binary purged
CLEAN_FILES += $(TEST_PROG)

