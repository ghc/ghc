#################################################################################
#
#			    GHC boilerplate.mk
#
#		Boilerplate Makefile for an fptools project
#
#################################################################################

# Begin by slurping in the boilerplate from one level up.
# Remember, TOP is the top level of the innermost level
# (FPTOOLS_TOP is the fptools top)

# We need to set TOP to be the TOP that the next level up expects!
# The TOP variable is reset after the inclusion of the fptools
# boilerplate, so we stash TOP away first:
GHC_TOP := $(TOP)
TOP:=$(TOP)/..

include $(TOP)/mk/boilerplate.mk

# Reset TOP
TOP:=$(GHC_TOP)

# -----------------------------------------------------------------
# Everything after this point
# augments or overrides previously set variables.
# (these files are optional, so `make' won't fret if
#  cannot get to them).
# -----------------------------------------------------------------

-include $(TOP)/mk/paths.mk
-include $(TOP)/mk/opts.mk
include $(TOP)/mk/suffix.mk

