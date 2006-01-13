ifeq "" "${MKDIR}"
MKDIR:=$(shell pwd)
#MKDIR:=$(PWD)
else
MKDIR:=$(patsubst %/$(notdir ${MKDIR}),%, ${MKDIR})
endif
include ${MKDIR}/Makefile.inc

