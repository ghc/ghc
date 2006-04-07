# Begin by slurping in the boilerplate from one level up.
# Remember, TOP is the top level of the innermost level
# (FPTOOLS_TOP is the fptools top)

# We need to set TOP to be the TOP that the next level up expects!
# The TOP variable is reset after the inclusion of the fptools
# boilerplate, so we stash TOP away first:
LIBRARIES_TOP := $(TOP)
TOP:=$(TOP)/..

# Pull in the fptools boilerplate
include $(TOP)/mk/boilerplate.mk

# Reset TOP
TOP:=$(LIBRARIES_TOP)

# -----------------------------------------------------------------
# Everything after this point
# augments or overrides previously set variables.

-include $(TOP)/mk/paths.mk
-include $(TOP)/mk/opts.mk
-include $(TOP)/mk/suffix.mk
