# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.3 2002/06/20 16:11:45 simonmar Exp $

TOP=../..
include $(TOP)/mk/boilerplate.mk

H_FILES = $(wildcard *.h)

includedir = $(libdir)/include
INSTALL_INCLUDES = $(H_FILES)

ifneq "$(HavePosixRegex)" "YES"
INSTALL_INCLUDES += ../cbits/regex/regex.h
endif

include $(TOP)/mk/target.mk
