# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.2 2002/02/14 15:14:02 simonmar Exp $

TOP=../..
include $(TOP)/mk/boilerplate.mk

H_FILES = $(wildcard *.h)

override datadir:=$(libdir)/include
INSTALL_DATAS = $(H_FILES)

ifneq "$(HavePosixRegex)" "YES"
INSTALL_DATAS += ../cbits/regex/regex.h
endif

include $(TOP)/mk/target.mk
