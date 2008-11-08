TOP=../..
include $(TOP)/mk/boilerplate.mk

SRC_HC_OPTS    += -fglasgow-exts -cpp -Wall
HS_PROG		= nofib-analyse

ifeq "$(ghc_ge_607)" "YES"
SRC_HC_OPTS     += -package containers
endif

SRC_HC_OPTS	+= -package regex-compat -package html

include $(TOP)/mk/target.mk
