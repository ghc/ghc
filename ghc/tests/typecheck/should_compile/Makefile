TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/should_compile.mk

SRC_HC_OPTS += -noC -dcore-lint -fno-warn-incomplete-patterns -hi-with-declarations -hi-with-instances -hi-with-exports

tc019_HC_OPTS = -fglasgow-exts
tc065_HC_OPTS = -syslib misc
tc087_HC_OPTS = -fglasgow-exts
tc089_HC_OPTS = -H12m
tc097_HC_OPTS = -fglasgow-exts
tc100_HC_OPTS = -O

# mkdependHS doesn't understand OPTIONS pragmas...
SRC_MKDEPENDHS_OPTS += -fglasgow-exts -syslib misc

include $(TOP)/mk/target.mk
