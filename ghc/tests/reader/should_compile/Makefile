TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/should_compile.mk

SRC_HC_OPTS += -noC -dcore-lint

read004_HC_OPTS = -fno-implicit-prelude
read006_HC_OPTS = -hi -ohi MyList.hi

include $(TOP)/mk/target.mk
