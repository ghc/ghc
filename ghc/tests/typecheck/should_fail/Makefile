TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/should_fail.mk

SRC_HC_OPTS += -noC

tcfail045_HC_OPTS = -fglasgow-exts
tcfail068_HC_OPTS = -fglasgow-exts
tcfail080_HC_OPTS = -fglasgow-exts

# mkdependHS doesn't understand OPTIONS pragmas...
SRC_MKDEPENDHS_OPTS += -fglasgow-exts

include $(TOP)/mk/target.mk
