# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------


# Build a program.  Invoke like this:
#
# utils/genapply_MODULES = Main
# utils/genapply_HC_OPTS = -package Cabal
# utils/genapply_dist_PROG = genapply
#
# $(eval $(call build-prog,utils/genapply,dist-install,1))

define build-prog
# $1 = dir
# $2 = distdir
# $3 = GHC stage to use (0 == bootstrapping compiler)

ifneq "$$(CLEANING)" "YES"
ifeq "$$($1_$2_PROG)" ""
$$(error $1_$2_PROG is not set)
endif
endif

ifeq "$$(findstring $3,0 1 2)" ""
$$(error $1/$2: stage argument to build-prog should be 0, 1, or 2)
endif

$(call clean-target,$1,$2,$1/$2)

ifneq "$$($1_$2_NOT_NEEDED)" "YES"
$$(eval $$(call build-prog-helper,$1,$2,$3))
endif
endef


define build-prog-helper
# $1 = dir
# $2 = distdir
# $3 = GHC stage to use (0 == bootstrapping compiler)

$(call all-target,$1,all_$1_$2)

ifeq "$$($1_USES_CABAL)" "YES"
$1_$2_USES_CABAL = YES
endif

ifeq "$$($1_$2_USES_CABAL)" "YES"
ifneq "$$(NO_INCLUDE_PKGDATA)" "YES"
include $1/$2/package-data.mk
endif
endif

$(call package-config,$1,$2,$3)

ifeq "$$($1_$2_USES_CABAL)$$($1_$2_VERSION)" "YES"
$1_$2_DISABLE = YES
endif

ifeq "$$($1_$2_DISABLE)" "YES"

ifeq "$$(DEBUG)" "YES"
$$(warning $1/$2 disabled)
endif

# The following code to build the package all depends on settings
# obtained from package-data.mk.  If we don't have package-data.mk
# yet, then don't try to do anything else with this package.  Make will
# try to build package-data.mk, then restart itself and we'll be in business.

$(call all-target,$1_$2,$1/$2/package-data.mk)

# We have a rule for package-data.mk only when the package is
# disabled, because we want the build to fail if we haven't run phase 0.
ifneq "$$(BINDIST)" "YES"
$(call build-package-data,$1,$2,$3)
endif

else

ifneq "$$(BINDIST)" "YES"
$1_$2_WAYS = v

$(call hs-sources,$1,$2)
$(call c-sources,$1,$2)

# --- DEPENDENCIES

$1_$2_depfile_base = $1/$2/build/.depend

$(call build-dependencies,$1,$2,$3)

# --- IMPLICIT RULES

# Just the 'v' way for programs
$(call distdir-way-opts,$1,$2,v,$3)

ifeq "$3" "0"
# For stage 0, we use GHC to compile C sources so that we don't have to
# worry about where the RTS header files are
$(call c-suffix-rules,$1,$2,v,YES)
else
$(call c-suffix-rules,$1,$2,v,NO)
endif

$(call hs-suffix-rules,$1,$2,v)
$$(foreach dir,$$($1_$2_HS_SRC_DIRS),\
  $$(eval $$(call hs-suffix-rules-srcdir,$1,$2,v,$$(dir))))

$(call c-objs,$1,$2,v)
$(call hs-objs,$1,$2,v)

ifeq "$$(BootingFromHc)" "NO"
$1/$2/build/tmp/$$($1_$2_PROG) : $$($1_$2_v_HS_OBJS) $$($1_$2_v_C_OBJS) $$($1_$2_v_S_OBJS) $$($1_$2_OTHER_OBJS) | $$$$(dir $$$$@)/.
	"$$($1_$2_HC)" -o $$@ $$($1_$2_v_ALL_HC_OPTS) $$(LD_OPTS) $$($1_$2_v_HS_OBJS) $$($1_$2_v_C_OBJS) $$($1_$2_v_S_OBJS) $$($1_$2_OTHER_OBJS)
else
$1/$2/build/tmp/$$($1_$2_PROG) : $$($1_$2_v_HS_OBJS) $$($1_$2_v_C_OBJS) $$($1_$2_v_S_OBJS) $$($1_$2_OTHER_OBJS) | $$$$(dir $$$$@)/.
	"$$(CC)" -o $$@ $$($1_$2_v_ALL_CC_OPTS) $$(LD_OPTS) $$($1_$2_v_HS_OBJS) $$($1_$2_v_C_OBJS) $$($1_$2_v_S_OBJS) $$($1_$2_OTHER_OBJS) $$($1_$2_v_EXTRA_CC_OPTS)
endif

# Note [lib-depends] if this program is built with stage1 or greater, we
# need to depend on the libraries too.  NB. since $(ALL_STAGE1_LIBS) and
# $(ALL_RTS_LIBS) are not defined until after libraries/*/ghc.mk have
# been included, this introduces an ordering dependency.
ifneq "$3" "0"
ifeq "$$(ALL_STAGE1_LIBS)" ""
$$(error ordering failure in $1: $$(ALL_STAGE1_LIBS) is empty)
endif
$1/$2/build/tmp/$$($1_$2_PROG) : $$(ALL_STAGE1_LIBS) $$(ALL_RTS_LIBS) $$(OTHER_LIBS)
endif
endif

ifeq "$$($1_$2_INSTALL_INPLACE)" "NO"
$(call all-target,$1_$2,$1/$2/build/tmp/$$($1_$2_PROG))
else
# Where do we install the inplace version?
ifeq "$$($1_$2_SHELL_WRAPPER) $$(Windows)" "YES NO"
$1_$2_INPLACE = $$(INPLACE_LIB)/$$($1_$2_PROG)
else
ifeq "$$($1_$2_TOPDIR)" "YES"
$1_$2_INPLACE = $$(INPLACE_TOPDIR)/$$($1_$2_PROG)
else
$1_$2_INPLACE = $$(INPLACE_BIN)/$$($1_$2_PROG)
endif
endif

$(call all-target,$1_$2,$$($1_$2_INPLACE))
$(call clean-target,$1,$2_inplace,$$($1_$2_INPLACE))

# INPLACE_BIN might be empty if we're distcleaning
ifeq "$(findstring clean,$(MAKECMDGOALS))" ""
$$($1_$2_INPLACE) : $1/$2/build/tmp/$$($1_$2_PROG) | $$$$(dir $$$$@)/.
	"$$(CP)" -p $$< $$@
	touch $$@
endif

# touch is necessary; cp doesn't update the file time.
endif

$(call shell-wrapper,$1,$2)

ifeq "$$($1_$2_INSTALL)" "YES"
ifeq "$$($1_$2_TOPDIR)" "YES"
INSTALL_TOPDIRS += $1/$2/build/tmp/$$($1_$2_PROG)
else
INSTALL_BINS += $1/$2/build/tmp/$$($1_$2_PROG)
endif
endif

endif # package-data.mk exists

endef
