TOP=../..
include $(TOP)/mk/boilerplate.mk

HS_PROG = ghctags
SRC_HC_OPTS += -package ghc
HC=$(GHC_STAGE1)

CLEAN_FILES += Main.hi

INSTALL_PROGS += $(HS_PROG)

include $(TOP)/mk/target.mk

