#
# target.mk project stub
#

# We need to set TOP to be the TOP that the next level up expects!
# The TOP variable is reset after the inclusion of the fptools
# boilerplate, so we stash TOP away first:
GHC_TOP := $(TOP)
TOP:=$(TOP)/..

# When booting from .hc files, remove the suffix rule for 
# .l?hs -> .o, so that the .hc -> .o is used instead.
# Also disable the generation of the .hc files, even if
# the .l?hs files are newer than the .hc ones.
ifeq "$(GhcWithHscBuiltViaC)" "YES"
%.$(way_)o  : %.lhs
%.$(way_)o  : %.hs
%.$(way_)hc : %.lhs
%.$(way_)hc : %.hs
endif

include $(TOP)/mk/target.mk

# Reset TOP
TOP:=$(GHC_TOP)
