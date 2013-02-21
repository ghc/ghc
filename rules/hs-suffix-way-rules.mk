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


define hs-suffix-way-rules  # args: $1 = dir,  $2 = distdir, $3 = way

ifneq "$$(BINDIST)" "YES"

$1/$2/build/%.$$($3_hcsuf) : $1/$2/build/%.hs $$(LAX_DEPS_FOLLOW) $$($1_$2_HC_DEP)
	$$(call cmd,$1_$2_HC) $$($1_$2_$3_ALL_HC_OPTS) -C $$< -o $$@

$1/$2/build/%.$$($3_osuf) : $1/$2/build/%.hs $$(LAX_DEPS_FOLLOW) $$($1_$2_HC_DEP)
	$$(call cmd,$1_$2_HC) $$($1_$2_$3_ALL_HC_OPTS) -c $$< -o $$@

$1/$2/build/%.$$($3_hcsuf) : $1/$2/build/autogen/%.hs $$(LAX_DEPS_FOLLOW) $$($1_$2_HC_DEP)
	$$(call cmd,$1_$2_HC) $$($1_$2_$3_ALL_HC_OPTS) -C $$< -o $$@

$1/$2/build/%.$$($3_osuf) : $1/$2/build/autogen/%.hs $$(LAX_DEPS_FOLLOW) $$($1_$2_HC_DEP)
	$$(call cmd,$1_$2_HC) $$($1_$2_$3_ALL_HC_OPTS) -c $$< -o $$@

endif

$$(foreach dir,$$($1_$2_HS_SRC_DIRS),\
  $$(eval $$(call hs-suffix-rules-srcdir,$1,$2,$3,$$(dir))))

$(call hi-rule,$1/$2/build,$1/$2/build,$3)
$(call hi-rule,$1/$2/build/autogen,$1/$2/build,$3)
$$(foreach dir,$$($1_$2_HS_SRC_DIRS),\
  $$(eval $$(call hi-rule,$1/$$(dir),$1/$2/build,$3)))

endef # hs-suffix-way-rules

