# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      https://gitlab.haskell.org/ghc/ghc/wikis/building/architecture
#      https://gitlab.haskell.org/ghc/ghc/wikis/building/modifying
#
# -----------------------------------------------------------------------------

#
# Substitute for 'touch' on win32 platforms (without an Unix toolset installed).
#
TOP=../..
include $(TOP)/mk/boilerplate.mk

C_SRCS=touchy.c
C_PROG=touchy
SRC_CC_OPTS += -O

#
# Install touchy in lib/.*
#
INSTALL_LIBEXECS += $(C_PROG)

include $(TOP)/mk/target.mk

# Get it over with!
boot :: all

binary-dist:
	$(INSTALL_DIR)               $(BIN_DIST_DIR)/utils/touchy
	$(INSTALL_DATA)    Makefile  $(BIN_DIST_DIR)/utils/touchy/
	$(INSTALL_PROGRAM) $(C_PROG) $(BIN_DIST_DIR)/utils/touchy/

