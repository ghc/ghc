# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.1 2004/01/15 14:43:22 igloo Exp $

TOP=..
include $(TOP)/mk/boilerplate.mk

ALL_DIRS     = Language/Haskell Language/Haskell/TH
PACKAGE      = template-haskell
PACKAGE_DEPS = base

Language/Haskell/TH/Syntax_HC_OPTS += -fglasgow-exts

SRC_HADDOCK_OPTS += -t "Haskell Hierarchical Libraries ($(PACKAGE) package)"

include $(TOP)/mk/target.mk
