# 
# This library contains modules only available in versions of GHC
# newer than the current one.  They are implemented as stubs that
# #include the actual code from fptools/libraries.
#
# The idea is to ease the task of writing portable code in GHC and its
# tools: the client can link with libghccompat.a and assume that all
# the modules are available.  In this way we can add modules to the library
# and start using them right away in GHC, as long as the new library modules
# can be compiled using older versions of GHC.
#

TOP=../..
include $(TOP)/mk/boilerplate.mk

ALL_DIRS = \
	Data \
	Distribution \
	Distribution/Compat

LIBRARY = libghccompat.a

SRC_HC_OPTS += -I$(FPTOOLS_TOP)/libraries -fglasgow-exts

include $(TOP)/mk/target.mk
