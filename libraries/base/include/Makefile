# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.6 2005/03/02 16:39:57 ross Exp $

TOP=../..
include $(TOP)/mk/boilerplate.mk
-include ../config.mk

H_FILES = $(wildcard *.h)

includedir = $(libdir)/include
INSTALL_INCLUDES = $(H_FILES)

ifneq "$(HavePosixRegex)" "YES"
INSTALL_INCLUDES += ./regex/regex.h
endif

DIST_CLEAN_FILES += HsBaseConfig.h

include $(TOP)/mk/target.mk
