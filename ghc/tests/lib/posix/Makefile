TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/should_run.mk

SRC_HC_OPTS += -dcore-lint -package posix -fglasgow-exts

# Posix007 is interactive, you'll have to issue a ^C on your tty.
posix007_RUNTEST_OPTS += -i/dev/tty

.PRECIOUS: %.o %.bin

include $(TOP)/mk/target.mk
