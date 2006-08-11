# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.2 2004/11/26 16:22:12 simonmar Exp $

TOP=..
include $(TOP)/mk/boilerplate.mk

ALL_DIRS     = Language/Haskell Language/Haskell/TH
PACKAGE      = template-haskell
VERSION	     = 2.0
PACKAGE_DEPS = base

Language/Haskell/TH/Syntax_HC_OPTS += -fglasgow-exts

SRC_HADDOCK_OPTS += -t "Haskell Hierarchical Libraries ($(PACKAGE) package)"

include $(TOP)/mk/target.mk
