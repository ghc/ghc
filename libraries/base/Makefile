# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.4 2001/07/31 16:41:32 simonmar Exp $

TOP=..
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------

ifeq "$(way)" ""
SUBDIRS = cbits
else
SUBDIRS=
endif

ALL_DIRS = \
	Control \
	Control/Concurrent \
	Control/Monad \
	Control/Monad/ST \
	Data \
	Data/Array \
	Database \
	Debug \
	FileFormat \
	Foreign \
	Foreign/C \
	Foreign/Marshal \
	GHC \
	Hugs \
	Language \
	Network \
	NHC \
	System \
	System/IO \
	Text \
	Text/Show

PKG=core

# dependencies between .hsc files
GHC/IO.hs : GHC/Handle.hs

GHC/Prim.$(way_)hi : GHC/Prim.hi-boot
	cp $< $@

lib : GHC/Prim.$(way_)hi

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk

