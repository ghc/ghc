TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/should_compile.mk

HC_OPTS += -noC -dcore-lint -fno-warn-incomplete-patterns -hi -ddump-tc

tc019_HC_OPTS = -fglasgow-exts
tc065_HC_OPTS = -syslib ghc

include $(TOP)/mk/target.mk
