# -----------------------------------------------------------------------------
# $Id: paths.mk,v 1.29 2001/03/05 10:29:00 simonmar Exp $
#
# ghc project specific make variables
#

#-----------------------------------------------------------------------------
# HsTags

ifdef UseInstalledUtils
HSTAGS	 		= hstags
else
HSTAGS	 		= $(HSTAGS_DIR)/hstags
HSTAGS_DIR 		= $(GHC_UTILS_DIR)/hstags
endif

#-----------------------------------------------------------------------------
# Extra things ``only for'' for the ghc project

GHC_DRIVER_DIR	    	:= $(TOP)/driver
GHC_COMPILER_DIR  	:= $(TOP)/compiler
GHC_RUNTIME_DIR   	:= $(TOP)/rts
GHC_LIB_DIR	 	:= $(TOP)/lib
GHC_INCLUDE_DIR   	:= $(TOP)/includes
GHC_UTILS_DIR	 	:= $(TOP)/utils
GHC_INTERPRETER_DIR 	:= $(TOP)/interpreter

GHC_UNLIT_DIR 		:= $(GHC_UTILS_DIR)/unlit
GHC_MANGLER_DIR 	:= $(GHC_DRIVER_DIR)/mangler
GHC_SPLIT_DIR	 	:= $(GHC_DRIVER_DIR)/split

GHC_UNLIT		= $(GHC_UNLIT_DIR)/unlit$(EXE_SUFFIX)
GHC_MANGLER		= $(GHC_MANGLER_DIR)/ghc-asm
GHC_SPLIT		= $(GHC_SPLIT_DIR)/ghc-split

GHC_SYSMAN    		= $(GHC_RUNTIME_DIR)/parallel/SysMan
GHC_SYSMAN_DIR 		= $(GHC_RUNTIME_DIR)/parallel

#-----------------------------------------------------------------------------
# Stuff for the C-compiling phase in particular...

#
# There's no need to compute dependencies when booting from .hc files
#
ifneq "$(GhcWithHscBuiltViaC)" "YES"
MKDEPENDHS_SRCS = $(HS_SRCS)
else
MKDEPENDHS_SRCS =
endif
