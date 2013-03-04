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


define package-config # args: $1 = dir, $2 = distdir, $3 = GHC stage
$(call trace, package-config($1,$2,$3))
$(call profStart, package-config($1,$2,$3))

$1_$2_HC = $$(GHC_STAGE$3)
$1_$2_CC = $$(CC_STAGE$3)
$1_$2_AS = $$(AS_STAGE$3)
$1_$2_AR = $$(AR_STAGE$3)
$1_$2_AR_OPTS = $$(AR_OPTS_STAGE$3)
$1_$2_EXTRA_AR_ARGS = $$(EXTRA_AR_ARGS_STAGE$3)
$1_$2_ArSupportsAtFile = $$(ArSupportsAtFile_STAGE$3)

# configuration stuff that depends on which GHC we're building with
ifeq "$3" "0"
$1_$2_HC_CONFIG = $$(GHC_STAGE0)
$1_$2_HC_CONFIG_DEP =
$1_$2_GHC_PKG = $$(GHC_PKG)
$1_$2_GHC_PKG_DEP = 
$1_$2_HC_MK_DEPEND = $$($1_$2_HC)
# We can't make rules depend on the bootstrapping compiler, as then
# on cygwin we get a dep on c:/ghc/..., and make gets confused by the :
$1_$2_HC_MK_DEPEND_DEP =
$1_$2_HC_DEP =
$1_$2_HC_PKGCONF = -$(GHC_PACKAGE_DB_FLAG) $$(BOOTSTRAPPING_CONF)
$1_$2_GHC_PKG_OPTS = --$(GHC_PACKAGE_DB_FLAG)=$$(BOOTSTRAPPING_CONF)
$1_$2_CONFIGURE_OPTS += --package-db=$$(TOP)/$$(BOOTSTRAPPING_CONF)
$1_$2_MORE_HC_OPTS += -no-user-$(GHC_PACKAGE_DB_FLAG)
$1_$2_MORE_HC_OPTS += -rtsopts
else
$1_$2_HC_PKGCONF = 
$1_$2_HC_CONFIG = $$(TOP)/$$(GHC_STAGE1)
$1_$2_HC_CONFIG_DEP = $$(GHC_STAGE1)
$1_$2_GHC_PKG = $$(TOP)/$$(ghc-pkg_INPLACE)
$1_$2_GHC_PKG_DEP = $$$$(ghc-pkg_INPLACE)
$1_$2_GHC_PKG_OPTS =
# If stage is not 0 then we always use stage1 for making .depend, as later
# stages aren't available early enough
$1_$2_HC_MK_DEPEND = $$(GHC_STAGE1)
$1_$2_HC_MK_DEPEND_DEP = $$($1_$2_HC_MK_DEPEND)
$1_$2_HC_DEP = $$($1_$2_HC)
$1_$2_MORE_HC_OPTS += -no-user-package-db
$1_$2_MORE_HC_OPTS += -rtsopts
endif

# Used by pretty_commands.mk
label_$1_$2_CC=CC
label_$1_$2_AS=AS
label_$1_$2_AR=AR
label_$1_$2_HC=HC [stage $3]
label_$1_$2_GHC_PKG=GHC PKG

# Useful later
$1_$2_SLASH_MODS = $$(subst .,/,$$($1_$2_MODULES))

$(call profEnd, package-config($1,$2,$3))
endef
