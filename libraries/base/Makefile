# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.5 2001/08/17 12:50:34 simonmar Exp $

TOP=..
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------

SUBDIRS = cbits include

ALL_DIRS = \
	Control \
	Control/Concurrent \
	Control/Monad \
	Control/Monad/ST \
	Data \
	Data/Array \
	Database \
	Debug \
	Debug/QuickCheck \
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
	Text/PrettyPrint \
	Text/Regex \
	Text/Show

PKG=core

# dependencies between .hsc files
GHC/IO.hs : GHC/Handle.hs

GHC/Prim.$(way_)hi : GHC/Prim.hi-boot
	cp $< $@

lib : GHC/Prim.$(way_)hi

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk

