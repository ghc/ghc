TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/should_compile.mk

SRC_HC_OPTS += -noC -dcore-lint

ds035_HC_OPTS = -fglasgow-exts
ds044_HC_OPTS = -O -funbox-strict-fields
ds050_HC_OPTS = -fglasgow-exts

# mkdependHS doesn't understand OPTIONS pragmas...
SRC_MKDEPENDHS_OPTS += -fglasgow-exts

include $(TOP)/mk/target.mk


