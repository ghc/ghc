TOP = ../..
include $(TOP)/mk/boilerplate.mk
SRC_HC_OPTS += -fglasgow-exts
include $(TOP)/mk/should_run.mk

HC_OPTS += -dcore-lint

.PRECIOUS: %.o %.bin
include $(TOP)/mk/target.mk

