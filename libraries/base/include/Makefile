# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.4 2004/10/05 07:43:07 mthomas Exp $

TOP=../..
include $(TOP)/mk/boilerplate.mk

H_FILES = $(wildcard *.h)

includedir = $(libdir)/include
INSTALL_INCLUDES = $(H_FILES)

ifneq "$(HavePosixRegex)" "YES"
INSTALL_INCLUDES += ./regex/regex.h
endif

include $(TOP)/mk/target.mk
