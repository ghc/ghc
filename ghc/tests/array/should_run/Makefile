TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/should_run.mk

SRC_HC_OPTS += -dcore-lint

arr014_HC_OPTS = -fglasgow-exts

arr003_RUNTEST_OPTS = -x 1
arr004_RUNTEST_OPTS = -x 1
arr007_RUNTEST_OPTS = -x 1
arr008_RUNTEST_OPTS = -x 1

# mkdependHS doesn't understand OPTIONS pragmas...
SRC_MKDEPENDHS_OPTS += -fglasgow-exts

include $(TOP)/mk/target.mk
