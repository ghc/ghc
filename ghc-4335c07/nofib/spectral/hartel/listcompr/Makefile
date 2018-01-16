TOP = ../../..
include $(TOP)/mk/boilerplate.mk

SRC_HC_OPTS += -cpp -fglasgow-exts

# no fast/norm/slow stuff: increasing the parameter would generate a
# huge .stdout file, bloating the repository and not testing anything
# particularly useful --SDM.

include $(TOP)/mk/target.mk

