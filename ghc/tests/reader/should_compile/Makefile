TOP = ../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/should_compile.mk

SRC_HC_OPTS += -noC -dcore-lint

read012_HC_OPTS += -O -funfold-casms-in-hi-file -fglasgow-exts -ohi T1.hi
read013_HC_OPTS += -O -funfold-casms-in-hi-file -fglasgow-exts
read014_HC_OPTS += -Wall
read022_HC_OPTS = -fglasgow-exts

include $(TOP)/mk/target.mk
