# -----------------------------------------------------------------------------
# $Id: paths.mk,v 1.37 2001/09/10 12:57:25 simonmar Exp $
#
# ghc project specific make variables
#

#-----------------------------------------------------------------------------
# HsTags


#-----------------------------------------------------------------------------
# Extra things ``only for'' for the ghc project
# 	These are all build-time things

GHC_INCLUDE_DIR   	:= $(TOP)/includes
GHC_COMPILER_DIR  	:= $(TOP)/compiler
GHC_RUNTIME_DIR   	:= $(TOP)/rts
GHC_LIB_DIR	 	:= $(TOP)/lib

# ---------------------------------------------------
# -- These variables are defined primarily so they can 
# -- be spat into Config.hs by ghc/compiler/Makefile
#
# -- See comments in ghc/compiler/main/SysTools.lhs 


PROJECT_DIR		:= ghc
GHC_DRIVER_DIR	    	:= $(PROJECT_DIR)/driver
GHC_UTILS_DIR	 	:= $(PROJECT_DIR)/utils

GHC_TOUCHY_DIR 		= $(GHC_UTILS_DIR)/touchy

GHC_UNLIT_DIR 		= $(GHC_UTILS_DIR)/unlit
GHC_UNLIT		= unlit$(EXE_SUFFIX)

GHC_MANGLER_DIR 	= $(GHC_DRIVER_DIR)/mangler
GHC_MANGLER		= ghc-asm

GHC_SPLIT_DIR	 	= $(GHC_DRIVER_DIR)/split
GHC_SPLIT		= ghc-split

GHC_SYSMAN_DIR 		= $(GHC_RUNTIME_DIR)/parallel
GHC_SYSMAN    		= SysMan

GHC_HSTAGS_DIR 		= $(GHC_UTILS_DIR)/hstags
GHC_HSTAGS 		= hstags

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"

GHC_CP			= "xcopy /y"
GHC_PERL		= perl
GHC_TOUCHY		= touchy$(EXE_SUFFIX)

else

GHC_CP			= $(CP)
GHC_PERL		= $(PERL)
GHC_TOUCHY		= touch

endif

