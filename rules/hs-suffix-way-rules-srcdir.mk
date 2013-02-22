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


define hs-suffix-way-rules-srcdir
# args: $1 = dir,  $2 = distdir, $3 = way, $4 = srcdir

ifneq "$$(BINDIST)" "YES"

# Compiling Haskell source

$1/$2/build/%.$$($3_osuf) : $1/$4/%.hs $$(LAX_DEPS_FOLLOW) $$($1_$2_HC_DEP) $$($1_$2_PKGDATA_DEP)
	$$(call cmd,$1_$2_HC) $$($1_$2_$3_ALL_HC_OPTS) -c $$< -o $$@ $$(if $$(findstring YES,$$($1_$2_DYNAMIC_TOO)),-dyno $$(addsuffix .$$(dyn_osuf),$$(basename $$@)))

$1/$2/build/%.$$($3_osuf) : $1/$4/%.lhs $$(LAX_DEPS_FOLLOW) $$($1_$2_HC_DEP) $$($1_$2_PKGDATA_DEP)
	$$(call cmd,$1_$2_HC) $$($1_$2_$3_ALL_HC_OPTS) -c $$< -o $$@ $$(if $$(findstring YES,$$($1_$2_DYNAMIC_TOO)),-dyno $$(addsuffix .$$(dyn_osuf),$$(basename $$@)))

$1/$2/build/%.$$($3_hcsuf) : $1/$4/%.hs $$(LAX_DEPS_FOLLOW) $$($1_$2_HC_DEP) $$($1_$2_PKGDATA_DEP)
	$$(call cmd,$1_$2_HC) $$($1_$2_$3_ALL_HC_OPTS) -C $$< -o $$@

$1/$2/build/%.$$($3_hcsuf) : $1/$4/%.lhs $$(LAX_DEPS_FOLLOW) $$($1_$2_HC_DEP) $$($1_$2_PKGDATA_DEP)
	$$(call cmd,$1_$2_HC) $$($1_$2_$3_ALL_HC_OPTS) -C $$< -o $$@

# XXX: for some reason these get used in preference to the direct
# .hs->.o rule, I don't know why --SDM

$1/$2/build/%.$$($3_osuf) : $1/$4/%.hc includes/ghcautoconf.h includes/ghcplatform.h | $$$$(dir $$$$@)/.
	$$(call cmd,$1_$2_CC) $$($1_$2_$3_ALL_CC_OPTS) $$(addprefix -I,$$(GHC_INCLUDE_DIRS)) -x c -c $$< -o $$@ $$(if $$(findstring YES,$$($1_$2_DYNAMIC_TOO)),-dyno $$(addsuffix .$$(dyn_osuf),$$(basename $$@)))

$1/$2/build/%.$$($3_osuf) : $1/$2/build/%.hc includes/ghcautoconf.h includes/ghcplatform.h
	$$(call cmd,$1_$2_CC) $$($1_$2_$3_ALL_CC_OPTS) $$(addprefix -I,$$(GHC_INCLUDE_DIRS)) -x c -c $$< -o $$@ $$(if $$(findstring YES,$$($1_$2_DYNAMIC_TOO)),-dyno $$(addsuffix .$$(dyn_osuf),$$(basename $$@)))

# $1/$2/build/%.$$($3_osuf) : $1/$2/build/%.$$($3_way_)hc
# 	$$(call cmd,$1_$2_HC) $$($1_$2_$3_ALL_HC_OPTS) -c $$< -o $$@
#
# $1/$2/build/%.$$($3_osuf) : $1/$2/build/%.hc
# 	$$(call cmd,$1_$2_HC) $$($1_$2_$3_ALL_HC_OPTS) -c $$< -o $$@
#
# $1/$2/build/%.$$($3_way_)s : $1/$2/build/%.$$($3_way_)hc
# 	$$(call cmd,$1_$2_HC) $$($1_$2_$3_ALL_HC_OPTS) -S $$< -o $$@

# Now the rules for hs-boot files.

$1/$2/build/%.$$($3_way_)o-boot : $1/$4/%.hs-boot $$(LAX_DEPS_FOLLOW) $$($1_$2_HC_DEP) $$($1_$2_PKGDATA_DEP)
	$$(call cmd,$1_$2_HC) $$($1_$2_$3_ALL_HC_OPTS) -c $$< -o $$@ $$(if $$(findstring YES,$$($1_$2_DYNAMIC_TOO)),-dyno $$(addsuffix .$$(dyn_osuf)-boot,$$(basename $$@)))

$1/$2/build/%.$$($3_way_)o-boot : $1/$4/%.lhs-boot $$(LAX_DEPS_FOLLOW) $$($1_$2_HC_DEP) $$($1_$2_PKGDATA_DEP)
	$$(call cmd,$1_$2_HC) $$($1_$2_$3_ALL_HC_OPTS) -c $$< -o $$@ $$(if $$(findstring YES,$$($1_$2_DYNAMIC_TOO)),-dyno $$(addsuffix .$$(dyn_osuf)-boot,$$(basename $$@)))

# stubs are automatically generated and compiled by GHC

$1/$2/build/%_stub.$$($3_osuf): $1/$2/build/%.$$($3_osuf)
	@:

endif

endef

