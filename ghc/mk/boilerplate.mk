# GHC boilerplate.mk

GHC_TOP := $(TOP)

# Include this first, because the top-level .mk files might depend on
# the values of $(ProjectXXX) variables.  (in fact they might/should not, 
# but we're not brave enough to move this include later --SDM).
-include $(GHC_TOP)/mk/version.mk

# We need to set TOP to be the TOP that the next level up expects!
# The TOP variable is reset after the inclusion of the fptools
# boilerplate, so we stash TOP away first:
TOP:=$(GHC_TOP)/..

include $(TOP)/mk/boilerplate.mk

# Reset TOP
TOP:=$(GHC_TOP)

# -----------------------------------------------------------------
# Everything after this point
# augments or overrides previously set variables.
# -----------------------------------------------------------------

-include $(GHC_TOP)/mk/config.mk
-include $(GHC_TOP)/mk/paths.mk
-include $(GHC_TOP)/mk/opts.mk
-include $(GHC_TOP)/mk/suffix.mk
