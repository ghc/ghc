#################################################################################
#
#			    nofib/mk/paths.mk
#
# 	This file defines Make variables for standard directories
#	and file lists
#
#################################################################################


# Define NOFIB_PROG and NOFIB_PROG_WAY.  
# In ..../nofib/imaginary/exp3_8, NOFIB_PROG is exp3_8 by default.
# and for way w			  NOFIB_PROG_WAY is exp3_8_w
#
NOFIB_PROG = $(notdir $(shell pwd))
NOFIB_PROG_WAY = $(NOFIB_PROG)$(_way)

# Eventually, have the binary purged
CLEAN_FILES += $(NOFIB_PROG_WAY)

#
# If tests are expected to end in compilation failure,
# set the flag HC_MAY_FAIL to YES (before including boilerplate,mk !)
#
# Options to the runstdtest that we wrap around the `real' HC below
# can be set through HC_RUNSTDTEST_OPTS
#
ifeq "$(HC_MAY_FAIL)" "YES"
HC:=$(RUNSTDTEST) -x1 $(HC_RUNSTDTEST_OPTS) -- $(HC)
endif
