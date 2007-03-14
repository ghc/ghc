TOP=../..
include $(TOP)/mk/boilerplate.mk

HS_PROG = genprimopcode

SRC_HC_OPTS += -Wall

ifeq "$(ghc_ge_504)" "NO"
SRC_HC_OPTS += -package text
endif

ifeq "$(ghc_ge_602)" "YES"
SRC_HC_OPTS += -package parsec
endif

# genprimopcode is needed to boot in ghc/compiler...
ifneq "$(BootingFromHc)" "YES"
boot :: all
endif

include $(TOP)/mk/target.mk
