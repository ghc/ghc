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


define c-suffix-rules 
# $1 = dir
# $2 = distdir
# $3 = way
# $4 = use GHC (YES/NO)

ifneq "$$(BINDIST)" "YES"

ifeq "$4" "YES"

$1/$2/build/%.$$($3_osuf) : $1/%.c $$(LAX_DEPS_FOLLOW) $$$$($1_$2_HC_DEP) | $$$$(dir $$$$@)/.
	$$(call cmd,$1_$2_HC) $$($1_$2_$3_GHC_CC_OPTS) -c $$< -o $$@

$1/$2/build/%.$$($3_osuf) : $1/$2/build/%.c $$(LAX_DEPS_FOLLOW) $$$$($1_$2_HC_DEP)
	$$(call cmd,$1_$2_HC) $$($1_$2_$3_GHC_CC_OPTS) -c $$< -o $$@

$1/$2/build/%.$$($3_osuf) : $1/$2/build/%.$$($3_way_)s $$(LAX_DEPS_FOLLOW) $$$$($1_$2_HC_DEP)
	$$(call cmd,$1_$2_HC) $$($1_$2_$3_GHC_CC_OPTS) -c $$< -o $$@

$1/$2/build/%.$$($3_osuf) : $1/%.S $$(LAX_DEPS_FOLLOW) $$$$($1_$2_HC_DEP) | $$$$(dir $$$$@)/.
	$$(call cmd,$1_$2_HC) $$($1_$2_$3_GHC_CC_OPTS) -c $$< -o $$@

$1/$2/build/%.$$($3_way_)s : $1/$2/build/%.c $$(LAX_DEPS_FOLLOW) $$$$($1_$2_HC_DEP)
	$$(call cmd,$1_$2_HC) $$($1_$2_$3_GHC_CC_OPTS) -S $$< -o $$@

$1/$2/build/%.$$($3_way_)s : $1/%.c $$(LAX_DEPS_FOLLOW) $$$$($1_$2_HC_DEP)
	$$(call cmd,$1_$2_HC) $$($1_$2_$3_GHC_CC_OPTS) -S $$< -o $$@

else

$1/$2/build/%.$$($3_osuf) : $1/%.c | $$$$(dir $$$$@)/.
	$$(call cmd,$1_$2_CC) $$($1_$2_$3_ALL_CC_OPTS) -c $$< -o $$@

$1/$2/build/%.$$($3_osuf) : $1/$2/build/%.c
	$$(call cmd,$1_$2_CC) $$($1_$2_$3_ALL_CC_OPTS) -c $$< -o $$@

$1/$2/build/%.$$($3_osuf) : $1/$2/build/%.$$($3_way_)s
	$$(call cmd,$1_$2_AS) $$($1_$2_$3_ALL_AS_OPTS) -o $$@ $$<

$1/$2/build/%.$$($3_osuf) : $1/%.S | $$$$(dir $$$$@)/.
	$$(call cmd,$1_$2_CC) $$($1_$2_$3_ALL_CC_OPTS) -c $$< -o $$@

$1/$2/build/%.$$($3_way_)s : $1/$2/build/%.c
	$$(call cmd,$1_$2_CC) $$($1_$2_$3_ALL_CC_OPTS) -S $$< -o $$@

endif

endif

endef

