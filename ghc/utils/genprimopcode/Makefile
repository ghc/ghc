TOP=../..
include $(TOP)/mk/boilerplate.mk

HS_PROG = genprimopcode

ghc_lt_504 = $(shell if (test $(GhcCanonVersion) -lt 504); then echo YES; else echo NO; fi)
ifeq "$(ghc_lt_504)" "YES"
SRC_HC_OPTS += -package text
endif

ghc_ge_601 = $(shell if (test $(GhcCanonVersion) -ge 601); then echo YES; else echo NO; fi)
ifeq "$(ghc_ge_601)" "YES"
SRC_HC_OPTS += -package parsec
endif

# genprimopcode is needed to boot in ghc/compiler...
ifneq "$(BootingFromHc)" "YES"
boot :: all
endif

include $(TOP)/mk/target.mk
