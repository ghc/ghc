TOP=../..
include $(TOP)/mk/boilerplate.mk

SRC_HC_OPTS    += -fglasgow-exts -cpp -Wall
HS_PROG		= nofib-analyse

ifeq "$(ghc_ge_605)" "YES"
SRC_HC_OPTS	+= -package regex-compat -package html
endif

include $(TOP)/mk/target.mk
