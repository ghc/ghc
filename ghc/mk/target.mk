#
# target.mk project stub
#

# We need to set TOP to be the TOP that the next level up expects!
# The TOP variable is reset after the inclusion of the fptools
# boilerplate, so we stash TOP away first:
GHC_TOP := $(TOP)
TOP:=$(TOP)/..

include $(TOP)/mk/target.mk

# Reset TOP
TOP:=$(GHC_TOP)
