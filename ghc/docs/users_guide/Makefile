TOP = ../..
include $(TOP)/mk/boilerplate.mk

DOC_SRCS = user.lit
SRC_TEXI2HTML_OPTS += -number -monolithic -invisible xbm

ifeq "$(BIN_DIST)" "1"
install :: html dvi info
endif

include $(TOP)/mk/target.mk
