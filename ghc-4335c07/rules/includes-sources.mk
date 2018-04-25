# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------


define includes-sources # args: $1 = dir, $2 = distdir

ifeq "$$($1_$2_INCLUDE_DIRS)" ""
$1_$2_INCLUDE_DIRS = .
endif

$1_$2_INSTALL_INCLUDES_SRCS :=\
    $$(foreach file,$$($1_$2_INSTALL_INCLUDES),\
        $$(firstword \
            $$(wildcard \
                $$(foreach dir,$$($1_$2_INCLUDE_DIRS),\
                    $1/$$(dir)/$$(file)))))
endef
