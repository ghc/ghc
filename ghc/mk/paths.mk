# -----------------------------------------------------------------------------
# $Id: paths.mk,v 1.17 2000/01/10 11:59:55 simonmar Exp $
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

GHC_SYSMAN_DIR 		:= $(GHC_RUNTIME_DIR)/gum
GHC_HSP_DIR 		:= $(GHC_HSC_DIR)
GHC_MKDEPENDHS_DIR	:= $(GHC_UTILS_DIR)/mkdependHS
GHC_HSCPP_DIR 		:= $(GHC_UTILS_DIR)/hscpp
GHC_HSC_DIR 		:= $(GHC_COMPILER_DIR)
GHC_UNLIT_DIR 		:= $(GHC_UTILS_DIR)/unlit

GHC_INPLACE		= $(GHC_DRIVER_DIR)/ghc-inplace
GHC_HSCPP    		= $(GHC_HSCPP_DIR)/hscpp
GHC_MKDEPENDHS 		= $(GHC_MKDEPENDHS_DIR)/mkdependHS-inplace
GHC_HSP    		= $(GHC_HSP_DIR)/hsp
GHC_HSC    		= $(GHC_HSC_DIR)/hsc
GHC_SYSMAN    		= $(GHC_RUNTIME_DIR)/gum/SysMan

UNLIT	 		= $(GHC_UNLIT_DIR)/unlit
GHC_UNLIT		= $(GHC_UNLIT_DIR)/unlit

#-----------------------------------------------------------------------------
# Stuff for the C-compiling phase in particular...

ifeq ($(HaveGcc), YES)
GHC_OPT_HILEV_ASM 		= $(WhatGccIsCalled)
else
GHC_OPT_HILEV_ASM 		= $(CC)
endif

#
# There's no need to compute dependencies when booting from .hc files
#
ifneq "$(GhcWithHscBuiltViaC)" "YES"
MKDEPENDHS_SRCS = $(HS_SRCS)
else
MKDEPENDHS_SRCS =
endif
