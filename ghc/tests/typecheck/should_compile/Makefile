TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/should_compile.mk

SRC_HC_OPTS += -noC -dcore-lint -fno-warn-incomplete-patterns -hi-with-declarations -hi-with-instances -hi-with-exports

tc019_HC_OPTS = -fglasgow-exts
tc065_HC_OPTS = -syslib misc
tc097_HC_OPTS = -fglasgow-exts

include $(TOP)/mk/target.mk
