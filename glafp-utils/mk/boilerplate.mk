################################################################################
#
#			    GHC boilerplate.mk
#
#		Boilerplate Makefile for an fptools project
#
################################################################################

# Begin by slurping in the boilerplate from one level up.
# Remember, TOP is the top level of the innermost level
# (FPTOOLS_TOP is the fptools top)

# We need to set TOP to be the TOP that the next level up expects!
GLAFP_UTILS_TOP := $(TOP)
TOP:=$(GLAFP_UTILS_TOP)/..

include $(TOP)/mk/boilerplate.mk

TOP:=$(GLAFP_UTILS_TOP)

# -----------------------------------------------------------------
# Everything after this point
# augments or overrides previously set variables.
# (these files are optional, so `make' won't fret if
#  cannot get to them).
# -----------------------------------------------------------------

#Not currently used: -include $(GLAFP_UTILS_TOP)/mk/paths.mk
#Not currently used: -include $(GLAFP_UTILS_TOP)/mk/suffix.mk

# No ways, please
WAYS=
