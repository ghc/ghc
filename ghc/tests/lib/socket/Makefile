TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/should_run.mk

HC_OPTS += -dcore-lint -syslib posix -fglasgow-exts

.PRECIOUS: %.o %.bin

include $(TOP)/mk/target.mk
