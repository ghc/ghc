
ifneq "$(findstring 3.7, $(MAKE_VERSION))" ""
ifeq "$(findstring 3.79.1, $(MAKE_VERSION))" ""
$(error GNU make version 3.79.1 or later is required.)
endif
endif

################################################################################
#
#	Layout of the source tree
#
################################################################################

# Here we provide defines for the various directories in the source tree,
# so we can move things around more easily.  A define $(GHC_FOO_DIR)
# indicates a directory relative to the top of the source tree.

GHC_UTILS_DIR           = utils
GHC_INCLUDE_DIRS        = includes includes/dist includes/dist-derivedconstants/header includes/dist-ghcconstants/header
GHC_RTS_DIR             = rts
GHC_DRIVER_DIR          = driver

GHC_UNLIT_DIR           = $(GHC_UTILS_DIR)/unlit
GHC_CABAL_DIR           = $(GHC_UTILS_DIR)/ghc-cabal
GHC_SPLIT_DIR           = $(GHC_DRIVER_DIR)/split
GHC_SYSMAN_DIR          = $(GHC_RTS_DIR)/parallel

INPLACE                 = inplace
INPLACE_BIN             = $(INPLACE)/bin
INPLACE_LIB             = $(INPLACE)/lib
INPLACE_TOPDIR          = $(INPLACE)/lib
INPLACE_MINGW           = $(INPLACE)/mingw
INPLACE_PERL            = $(INPLACE)/perl

################################################################################
#
#    Bindist testing directory
#
################################################################################

BIN_DIST_INST_SUBDIR = "install   dir"
BIN_DIST_INST_DIR = bindisttest/$(BIN_DIST_INST_SUBDIR)

################################################################################
#
#    rm
#
################################################################################

# These are here, rather than in config.mk, as they need to exist in an
# unconfigured tree so that the various clean targets can be used
# without configuring:
ifeq "$(ONLY_SHOW_CLEANS)" "YES"
RM = utils/testremove/wouldrm
RM_OPTS = CLEAN_FILES
RM_OPTS_REC = CLEAN_REC
else
RM = rm
RM_OPTS = -f
RM_OPTS_REC = -rf
endif

# If $1 is empty then we don't do anything (as "rm -rf" fails on
# Solaris; trac #4916).
# If $1 contains a * then we fail; globbing needs to be done at the call
# site using $(wildcard ...). This makes it a little safer, as it's
# harder to accidentally delete something you didn't mean to.
# Similarly, we fail if any argument contains ".." or starts with a "/".

removeFiles = $(call removeHelper,removeFiles,"$(RM)",$(RM_OPTS),$1)
removeTrees = $(call removeHelper,removeTrees,"$(RM)",$(RM_OPTS_REC),$1)

removeHelper = $(if $(strip $4),\
                   $(if $(findstring *,$4),\
                       $(error $1: Got a star: $4),\
                   $(if $(findstring ..,$4),\
                       $(error $1: Got dot-dot: $4),\
                   $(if $(filter /%,$4),\
                       $(error $1: Got leading slash: $4),\
                       $2 $3 $4\
                    )))\
                )

