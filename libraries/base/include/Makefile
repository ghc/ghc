# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.5 2005/02/01 00:52:22 ross Exp $

TOP=../..
include $(TOP)/mk/boilerplate.mk
-include ../config.mk

H_FILES = $(wildcard *.h)

includedir = $(libdir)/include
INSTALL_INCLUDES = $(H_FILES)

ifneq "$(HavePosixRegex)" "YES"
INSTALL_INCLUDES += ./regex/regex.h
endif

include $(TOP)/mk/target.mk
