TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/should_compile.mk

SRC_HC_OPTS += -noC -dcore-lint

read004_HC_OPTS = -fno-implicit-prelude
read012_HC_OPTS += -O -funfold-casms-in-hi-file -fglasgow-exts
read013_HC_OPTS += -O -funfold-casms-in-hi-file -fglasgow-exts
read014_HC_OPTS += -Wall

include $(TOP)/mk/target.mk
